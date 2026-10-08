import { chromium } from "playwright";
import fs from "fs";
import path from "path";
import { fileURLToPath } from "url";

// Browser end-to-end test: entry login -> league picker -> signed iframe ->
// league dashboard scoped to the customer's manager.
//
// Env:
//   E2E_EMAIL, E2E_PASSWORD        (required) credentials for a test customer
//   ENTRY_URL                      (default https://shaggycamel-nba-shiny-entry.hf.space/)
//   E2E_EXPECT_COMPETITORS         (optional) JSON map {"ESPN:<league_id>":"<competitor_id>"}
//   E2E_HEADLESS                   (default "1"; set "0" to watch)

const ENTRY = process.env.ENTRY_URL || "https://shaggycamel-nba-shiny-entry.hf.space/";
const EMAIL = process.env.E2E_EMAIL;
const PASSWORD = process.env.E2E_PASSWORD;
const EXPECTED = process.env.E2E_EXPECT_COMPETITORS
  ? JSON.parse(process.env.E2E_EXPECT_COMPETITORS)
  : {};
const HEADLESS = process.env.E2E_HEADLESS !== "0";

if (!EMAIL || !PASSWORD) {
  console.error("set E2E_EMAIL and E2E_PASSWORD");
  process.exit(2);
}

const HERE = path.dirname(fileURLToPath(import.meta.url));
const SHOTS = path.join(HERE, "shots");
fs.mkdirSync(SHOTS, { recursive: true });

const results = [];
const consoleErrors = [];

const browser = await chromium.launch({ channel: "chrome", headless: HEADLESS });
const page = await browser.newPage({ viewport: { width: 1440, height: 900 } });

page.on("console", (m) => {
  if (m.type() === "error") consoleErrors.push(m.text());
});
page.on("requestfailed", (r) =>
  consoleErrors.push(`reqfail: ${r.url()} ${r.failure()?.errorText}`)
);

// The picker is a selectize control; the native <select> is hidden, so set the
// value through selectize rather than Playwright's selectOption.
const selectLeague = (value) =>
  page.evaluate((val) => {
    const el = document.querySelector("select#league");
    if (el && el.selectize && typeof el.selectize.setValue === "function") {
      el.selectize.setValue(val, false);
    } else {
      el.value = val;
      el.dispatchEvent(new Event("change", { bubbles: true }));
    }
  }, value);

try {
  await page.goto(ENTRY, { waitUntil: "domcontentloaded", timeout: 60000 });
  await page.waitForSelector("#email", { timeout: 60000 });
  await page.fill("#email", EMAIL);
  await page.fill("#password", PASSWORD);
  await page.click("#login");

  await Promise.race([
    page.waitForSelector("select#league", { timeout: 60000, state: "attached" }),
    page
      .waitForSelector("#login_message:not(:empty)", { timeout: 60000 })
      .then(async () => {
        throw new Error("login failed: " + (await page.textContent("#login_message")));
      }),
  ]);

  // selectize may hide non-selected options from the native select; prefer its
  // own option set when available.
  const leagueList = await page.evaluate(() => {
    const el = document.querySelector("select#league");
    if (el && el.selectize) return Object.keys(el.selectize.options);
    return Array.from(el.options).map((o) => o.value);
  });
  console.log("leagues offered:", JSON.stringify(leagueList));
  await page.screenshot({ path: path.join(SHOTS, "shell.png") });

  for (const value of leagueList) {
    const entry = { league: value, ok: false, notes: [] };
    try {
      await selectLeague(value);

      const leagueId = value.split(":")[1];
      await page.waitForFunction(
        (id) => {
          const f = document.querySelector("iframe");
          return !!f && f.src.includes("sig=") && f.src.includes(`league_id=${id}`);
        },
        leagueId,
        { timeout: 30000 }
      );
      const src = await page.$eval("iframe", (f) => f.src);
      const q = new URL(src).searchParams;
      entry.competitor_id = q.get("competitor_id");
      entry.has_sig = !!q.get("sig");
      entry.has_exp = !!q.get("exp");
      if (!entry.has_sig || !entry.has_exp) entry.notes.push("missing sig/exp");
      if (EXPECTED[value] && entry.competitor_id !== EXPECTED[value])
        entry.notes.push(`competitor ${entry.competitor_id} != expected ${EXPECTED[value]}`);

      const origin = new URL(src).origin;
      let frame = null;
      for (let i = 0; i < 60 && !frame; i++) {
        frame = page.frames().find((fr) => fr.url().startsWith(origin));
        if (!frame) await page.waitForTimeout(1000);
      }

      if (!frame) {
        entry.notes.push("league frame not found");
      } else {
        let verdict = "unknown";
        for (let i = 0; i < 90; i++) {
          const body = await frame.locator("body").innerText().catch(() => "");
          if (/Sign in required/i.test(body)) {
            verdict = "sign-in-required";
            break;
          }
          const title = await frame
            .locator("#navbar_title, .navbar-brand, [id^=navbar_title]")
            .first()
            .innerText()
            .catch(() => "");
          if (title && title.trim()) {
            verdict = "dashboard:" + title.trim();
            break;
          }
          await page.waitForTimeout(1000);
        }
        entry.verdict = verdict;
        if (verdict === "sign-in-required") entry.notes.push("handoff rejected");
        if (verdict === "unknown") entry.notes.push("no dashboard/modal detected");
        entry.ok = verdict.startsWith("dashboard:") && entry.notes.length === 0;
      }

      await page.screenshot({
        path: path.join(SHOTS, value.replace(/[^a-z0-9]+/gi, "_") + ".png"),
      });
    } catch (e) {
      entry.notes.push("error: " + e.message);
    }
    console.log(JSON.stringify(entry));
    results.push(entry);
  }
} catch (e) {
  console.log("FATAL: " + e.message);
  await page.screenshot({ path: path.join(SHOTS, "fatal.png") }).catch(() => {});
  process.exitCode = 2;
}

console.log("=== SUMMARY ===");
console.log(JSON.stringify({ results, consoleErrors: consoleErrors.slice(0, 20) }, null, 2));

await browser.close();
process.exitCode = results.some((r) => !r.ok) ? 1 : 0;
