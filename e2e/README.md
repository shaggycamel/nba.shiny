# Browser end-to-end test

Proves the cross-container flow that ties the deploy together:

```
entry (auth) ──signed iframe URL──▶ league container (NBA_REQUIRE_HANDOFF=1)
```

The entry point authenticates the customer, resolves their manager
(`competitor_id`) from `fty.customer_league`, and renders the league container in
an iframe with a signed, expiring URL. League containers run in strict mode
(`NBA_REQUIRE_HANDOFF=1`): a request without a valid token shows "Sign in
required" instead of the dashboard. The signature is the app-level security
boundary — Hugging Face does not enforce it.

This test drives a real browser to confirm login → pick league → signed iframe →
dashboard renders *as the mapped manager*, and that a bad/missing token is
rejected.

## Run

```bash
cd e2e
npm install                       # playwright (set PLAYWRIGHT_SKIP_BROWSER_DOWNLOAD=1
                                  # if you'll use the installed Chrome channel)

E2E_EMAIL='<test-customer-email>' \
E2E_PASSWORD='<password>' \
E2E_EXPECT_COMPETITORS='{"ESPN:123456":"7"}' \
  npm test
```

- Uses the installed **Google Chrome** (`channel: "chrome"`), so no Playwright
  browser download is required.
- `E2E_EXPECT_COMPETITORS` is optional; when set, each selected league's iframe
  `competitor_id` is checked against the expectation.
- Screenshots and a JSON summary (including console errors) are written under
  `e2e/shots/`. Exit code is non-zero if any league fails.
- `E2E_HEADLESS=0` runs headed.
- Credentials must be supplied via env and must **not** be committed. The test
  customer's `fty.customer.password_hash` must be set (the real-password path;
  the `NBA_ENTRY_DEV` bypass is off on hosted Spaces).

## Implementation notes (gotchas)

- The league picker is a **selectize** control: the native `select#league` is
  `display:none`, so Playwright's `selectOption` does not work. Set the value via
  `select#league.selectize.setValue(value, false)`.
- selectize may remove non-selected options from the native `<select>`; read the
  offered leagues from `selectize.options`, not from the `<select>` DOM.
- The `<iframe>` element is reused across league switches — wait for its `src` to
  contain both `sig=` and `league_id=<id>` before reading it.
- Cross-origin frames are readable by Playwright; pick the frame whose URL starts
  with the league Space origin.
- Success signal inside the league frame: a navbar title of the form
  `"<league> - <competitor>"`. Failure signal: the body contains
  `"Sign in required"`.

## Known open issue

A run against a test customer whose `fty.customer_league` had **four** 2025-26
leagues showed only **one** in the entry picker, while
`nba.shiny.entry::get_customer_leagues()` returned four when run inside the entry
image against the nuc's credentials INI. Hypotheses to resolve:

1. selectize hiding non-selected options from the native `<select>` (benign — the
   harness now reads `selectize.options`, so re-running may just show all four).
2. The entry Space's `NBA_DB_*` secrets pointing at a different database/schema
   than the deploy host's INI, where the customer has only one mapping.

Re-run and inspect `leagues offered:` in the output to distinguish the two before
changing any config. See `docs/NUC_DEPLOY.md` for Space secrets/variables.
