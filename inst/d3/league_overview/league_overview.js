// league_overview.js
// r2d3 script — multi-competitor line chart tracking a stat (value or rank)
// across matchup periods, with dense precalculated curve points for smooth
// lines and sparse integer-matchup points for markers/tooltips.
//
// Expected input data shape (long format, one row per competitor per
// matchup_sigmoid value):
//   [{
//     competitor_name: "...",
//     matchup_sigmoid: 17.0,      // dense x-value, float
//     matchup: 17,                 // integer matchup period (meaningful on marker rows)
//     value: 42.5,                 // the selected category's value or rank,
//                                   // already resolved in R to one consistent key
//     value_text: "...",           // tooltip text (sparse rows only)
//     is_point: true               // flags rows that sit on an exact integer
//                                   // matchup_sigmoid -- these get markers + tooltips
//   }, ...]
//
// options:
//   is_rank      : boolean — if true, y-axis is reversed (rank 1 at top)
//   y_label      : string — y-axis label / category name
//   competitors  : array of competitor names in fixed legend/color order
//                  (mirrors the stackedbar pattern — pass explicit order
//                  from R rather than inferring, since color/legend
//                  consistency across re-renders matters here too)
//   highlight_only : optional array of competitor names to keep fully
//                  visible; all others are dimmed (used for the "Just H2H"
//                  toggle). If omitted/empty, everyone is shown normally.

// ---- persistent setup (created once, reused across re-renders) --------

var margin = options.margin || { top: 48, right: 160, bottom: 40, left: 50 };

// Mini overview chart + brush reserve a fixed strip at the bottom of the
// total height, separate from margin.bottom (which still applies to the
// main chart's own x-axis labels).
var miniChartHeight = 50;
var miniChartGap = 35; // space between main chart's x-axis labels and the mini chart strip

var g = svg.select("g.plot-area");
if (g.empty()) {
  svg.style("overflow", "hidden");
  g = svg.append("g").attr("class", "plot-area");
  svg.append("g").attr("class", "x-axis");
  svg.append("g").attr("class", "y-axis");
  svg.append("defs").append("clipPath")
    .attr("id", "main-plot-clip")
    .append("rect");
  svg.append("g").attr("class", "lines-group").attr("clip-path", "url(#main-plot-clip)");
  svg.append("g").attr("class", "points-group").attr("clip-path", "url(#main-plot-clip)");
  svg.append("g").attr("class", "legend-group");
  svg.append("text").attr("class", "chart-title");
  svg.append("g").attr("class", "mini-lines-group");
  svg.append("g").attr("class", "mini-x-axis");
  svg.append("g").attr("class", "brush-group");
}

var xAxisG = svg.select(".x-axis");
var yAxisG = svg.select(".y-axis");
var linesG = svg.select(".lines-group");
var pointsG = svg.select(".points-group");
var legendG = svg.select(".legend-group");
var titleText = svg.select(".chart-title");
var miniLinesG = svg.select(".mini-lines-group");
var miniXAxisG = svg.select(".mini-x-axis");
var brushG = svg.select(".brush-group");

// Tooltip — same pattern as stackedbar.js: keyed to this svg node so
// re-renders reuse it rather than creating a new floating div each time.
var svgNode = svg.node();
var tooltip;
if (svgNode.__r2d3Tooltip) {
  tooltip = svgNode.__r2d3Tooltip;
} else {
  tooltip = d3.select("body").append("div")
    .style("position", "absolute")
    .style("pointer-events", "none")
    .style("z-index", "2147483647")
    .style("background", "rgba(255,255,255,0.97)")
    .style("border", "1px solid #ccc")
    .style("border-radius", "4px")
    .style("padding", "6px 10px")
    .style("font-size", "12px")
    .style("box-shadow", "0 1px 4px rgba(0,0,0,0.15)")
    .style("overflow", "hidden")
    .style("white-space", "nowrap")
    .style("transition", "width 180ms ease-out, height 180ms ease-out, opacity 120ms ease-out")
    .style("width", "0px")
    .style("height", "0px")
    .style("opacity", 0);
  svgNode.__r2d3Tooltip = tooltip;
}

// Fixed 12-ish color palette — Set2 only has 8 distinct hues, so for ~12
// competitors we extend with d3.schemeSet3 to avoid repeats. If you have
// a fixed competitor roster, consider passing explicit colors via options
// instead of relying on this palette order.
var palette = d3.schemeSet2.concat(d3.schemeSet3);

r2d3.onRender(function(data, svg, width, height, options) {
  var innerWidth = width - margin.left - margin.right;
  var mainInnerHeight = height - margin.top - margin.bottom - miniChartHeight - miniChartGap;
  var innerHeight = mainInnerHeight; // kept for readability in existing code below

  g.attr("transform", "translate(" + margin.left + "," + margin.top + ")");
  linesG.attr("transform", "translate(" + margin.left + "," + margin.top + ")");
  pointsG.attr("transform", "translate(" + margin.left + "," + margin.top + ")");

  svg.select("#main-plot-clip rect")
    .attr("x", 0)
    .attr("y", 0)
    .attr("width", innerWidth)
    .attr("height", mainInnerHeight);

  var isRank = !!options.is_rank;
  var competitors = (Array.isArray(options.competitors) && options.competitors.length > 0)
    ? options.competitors
    : Array.from(new Set(data.map(function(d) { return d.competitor_name; })));

  // ---- click/highlight state ----
  // offSet: competitors currently dimmed. Persists across re-renders
  // (e.g. category/toggle changes that don't touch H2H) on svgNode, but
  // gets replaced wholesale whenever the R-driven highlight_only option
  // actually changes — that's what makes toggling "Just H2H" reset any
  // manual single/double-click state, per spec.

  var h2hList = Array.isArray(options.highlight_only) ? options.highlight_only : [];
  var h2hKey = JSON.stringify(h2hList.slice().sort());

  if (svgNode.__lastH2hKey !== h2hKey) {
    svgNode.__lastH2hKey = h2hKey;
    svgNode.__offSet = h2hList.length > 0
      ? competitors.filter(function(c) { return h2hList.indexOf(c) === -1; }) // H2H on: everyone except the pair is off
      : []; // H2H off (or just changed off): clear off-set entirely, show everyone
  }

  var offSet = svgNode.__offSet;

  function isOff(name) {
    return offSet.indexOf(name) !== -1;
  }

  function opacityFor(name) {
    return isOff(name) ? 0.12 : 1;
  }

  var color = d3.scaleOrdinal()
    .domain(competitors)
    .range(palette.slice(0, competitors.length).map(function(c) { return d3.color(c).darker(0.6).toString(); }));

  // ---- scales ----

  var matchupValues = Array.from(new Set(
    data.filter(function(d) { return d.is_point; }).map(function(d) { return d.matchup; })
  )).sort(function(a, b) { return a - b; });

  var fullDomain = [matchupValues[0], matchupValues[matchupValues.length - 1]]; // pinned to real matchup range, not sigmoid's smoothed overshoot

  // `x` is the main chart's scale — its domain gets rescaled by the brush,
  // so it's declared with `var` (not `const`) and reassigned in place by
  // updateMainXDomain() below rather than recreated each brush event.
  var xPad = 6; // keeps the leftmost/rightmost marker's own radius from clipping at the plot edge
  var x = d3.scaleLinear()
    .domain(fullDomain)
    .range([xPad, innerWidth - xPad]);

  var yExtent = d3.extent(data, function(d) { return d.value; });
  var yPad = 6; // keeps the topmost/bottommost marker's own radius from clipping at the plot edge
  var y = d3.scaleLinear()
    .domain(isRank ? [yExtent[1], yExtent[0]] : yExtent) // reversed domain for rank: 1 at top
    .nice()
    .range([innerHeight - yPad, yPad]);

  // ---- title ----

  titleText
    .attr("x", margin.left)
    .attr("y", margin.top / 2)
    .style("font-size", "16px")
    .style("font-family", "sans-serif")
    .text(options.title || "");

  // ---- group data by competitor for line drawing (shared by main + mini) ----

  var byCompetitor = d3.group(data, function(d) { return d.competitor_name; });

  var lineSeries = competitors.map(function(name) {
    var rows = byCompetitor.get(name) || [];
    rows = rows.slice().sort(function(a, b) { return a.matchup_sigmoid - b.matchup_sigmoid; });
    return { name: name, rows: rows };
  });

  var pointRows = data.filter(function(d) { return d.is_point; });

  // ======================================================================
  // MAIN CHART — axis, lines, markers. Pulled into a function so the
  // brush can call it again with a new x-domain without re-running the
  // data reshape/scale-setup above (no new r2d3() call needed; this is a
  // pure client-side rescale of the same data).
  // ======================================================================

  var lineGen = d3.line()
    .curve(d3.curveLinear) // data is pre-interpolated; straight segments between dense points
    .x(function(d) { return x(d.matchup_sigmoid); })
    .y(function(d) { return y(d.value); });

  var hoveredIsTop = false;
  var measuredWidth = 0;
  var measuredHeight = 0;

  var pathsMerged, markersMerged; // assigned inside redrawMain(), referenced by legend click handlers below

  function redrawMain(animate) {
    var dur = animate ? 300 : 0;

    // Ticks: only label matchup values that fall within the current
    // (possibly brushed) x-domain, so the axis doesn't show ticks for
    // periods that have been scrolled/zoomed out of view.
    var domain = x.domain();
    var visibleTicks = matchupValues.filter(function(m) { return m >= domain[0] && m <= domain[1]; });

    xAxisG
      .attr("transform", "translate(" + margin.left + "," + (margin.top + innerHeight) + ")")
      .transition().duration(dur)
      .call(d3.axisBottom(x).tickValues(visibleTicks).tickFormat(d3.format("d")));

    yAxisG
      .attr("transform", "translate(" + margin.left + "," + margin.top + ")")
      .transition().duration(dur)
      .call(d3.axisLeft(y));

    // ---- lines ----

    var paths = linesG.selectAll("path.competitor-line")
      .data(lineSeries, function(d) { return d.name; });

    paths.exit().remove();

    var pathsEnter = paths.enter()
      .append("path")
      .attr("class", "competitor-line")
      .attr("fill", "none")
      .attr("stroke-width", 2);

    pathsMerged = pathsEnter.merge(paths);

    pathsMerged
      .attr("stroke", function(d) { return color(d.name); })
      .style("opacity", function(d) { return opacityFor(d.name); })
      .transition().duration(dur)
      .attr("d", function(d) { return lineGen(d.rows); });

    // ---- markers ----

    var markers = pointsG.selectAll("circle.marker-point")
      .data(pointRows, function(d) { return d.competitor_name + "-" + d.matchup; });

    markers.exit().remove();

    markersMerged = markers.enter()
      .append("circle")
      .attr("class", "marker-point")
      .attr("r", 3)
      .merge(markers);

    markersMerged
      .attr("fill", function(d) { return color(d.competitor_name); })
      .style("opacity", function(d) { return opacityFor(d.competitor_name); })
      .transition().duration(dur)
      .attr("cx", function(d) { return x(d.matchup_sigmoid); })
      .attr("cy", function(d) { return y(d.value); });

    // Hover handlers re-bound on every redraw (cheap for marker counts
    // here, and ensures closures always reference the current x/y scales
    // and offSet rather than stale ones from a previous redraw).
    markersMerged
      .on("mouseover", function(event, d) {
        if (d.value_text == null || isOff(d.competitor_name)) return;

        // Use the marker's vertical position within the plot area (not
        // stack position, since there's no stacking here) to decide
        // cascade direction: top half cascades down, bottom half cascades up.
        hoveredIsTop = y(d.value) < innerHeight / 2;

        var html = String(d.value_text).replace(/\n/g, "<br>");

        tooltip
          .style("transition", "none")
          .style("width", "auto")
          .style("height", "auto")
          .style("opacity", 0)
          .html(html);

        measuredWidth = tooltip.node().offsetWidth;
        measuredHeight = tooltip.node().offsetHeight;

        tooltip
          .style("width", "0px")
          .style("height", "0px");

        tooltip.node().offsetHeight; // force layout flush before re-enabling transition

        tooltip
          .style("transition", "width 180ms ease-out, height 180ms ease-out, opacity 120ms ease-out")
          .style("width", measuredWidth + "px")
          .style("height", measuredHeight + "px")
          .style("opacity", 1);
      })
      .on("mousemove", function(event) {
        var viewportTop = window.scrollY;
        var viewportBottom = window.scrollY + window.innerHeight;
        var viewportLeft = window.scrollX;
        var viewportRight = window.scrollX + window.innerWidth;

        var left = event.pageX + 12;
        var topPos = hoveredIsTop ? (event.pageY + 12) : (event.pageY - 12 - measuredHeight);

        left = Math.max(viewportLeft, Math.min(left, viewportRight - measuredWidth));
        topPos = Math.max(viewportTop, Math.min(topPos, viewportBottom - measuredHeight));

        tooltip
          .style("left", left + "px")
          .style("top", topPos + "px");
      })
      .on("mouseout", function() {
        tooltip
          .style("transition", "width 180ms ease-out, height 180ms ease-out, opacity 120ms ease-out")
          .style("opacity", 0)
          .style("width", "0px")
          .style("height", "0px");
      });
  }

  redrawMain(true);

  // ======================================================================
  // MINI OVERVIEW CHART — compressed, faint lines spanning the full
  // season always (not affected by the brush — it's what you drag on top
  // of). No axes/markers/tooltips on the lines themselves; only a
  // lightweight bottom axis for orientation.
  // ======================================================================

  var miniTop = margin.top + innerHeight + miniChartGap;

  miniLinesG.attr("transform", "translate(" + margin.left + "," + miniTop + ")");

  var xMini = d3.scaleLinear()
    .domain(fullDomain)
    .range([0, innerWidth]);

  // Mini chart uses the same y-domain as the main chart so its shape is a
  // faithful (just compressed) preview of the full lines.
  var yMini = d3.scaleLinear()
    .domain(y.domain())
    .range([miniChartHeight, 0]);

  var miniLineGen = d3.line()
    .curve(d3.curveLinear)
    .x(function(d) { return xMini(d.matchup_sigmoid); })
    .y(function(d) { return yMini(d.value); });

  var miniPaths = miniLinesG.selectAll("path.mini-line")
    .data(lineSeries, function(d) { return d.name; });

  miniPaths.exit().remove();

  miniPaths.enter()
    .append("path")
    .attr("class", "mini-line")
    .attr("fill", "none")
    .attr("stroke-width", 1)
    .merge(miniPaths)
    .attr("stroke", function(d) { return color(d.name); })
    .style("opacity", 0.35) // intentionally faint, matching the original rangeslider's overview look
    .attr("d", function(d) { return miniLineGen(d.rows); });

  miniXAxisG
    .attr("transform", "translate(" + margin.left + "," + (miniTop + miniChartHeight) + ")")
    .call(d3.axisBottom(xMini).tickValues(matchupValues).tickFormat(d3.format("d")).tickSizeOuter(0))
    .selectAll("text")
    .style("font-size", "9px");

  // ---- brush ----

  brushG.attr("transform", "translate(" + margin.left + "," + miniTop + ")");

  // Default brush extent: last ~5 matchup periods, regardless of the
  // W2W/Cum toggle. Only applied on first render or when the underlying
  // data's matchup range actually changes (e.g. switching league/category)
  // — once the user has dragged the brush, their selection persists across
  // unrelated re-renders (same pattern as offSet/click state above).
  var domainKey = JSON.stringify(fullDomain);

  if (svgNode.__lastDomainKey !== domainKey) {
    svgNode.__lastDomainKey = domainKey;
    svgNode.__brushDefaultMatchup = [Math.max(fullDomain[0], fullDomain[1] - 5), fullDomain[1]];
  }

  var defaultMatchupExtent = svgNode.__brushDefaultMatchup;
  var defaultPixelExtent = [xMini(defaultMatchupExtent[0]), xMini(defaultMatchupExtent[1])];

  function updateMainXDomain(matchupExtent) {
    x.domain(matchupExtent);
    redrawMain(false); // no transition during drag — keeps it responsive
  }

  // Inverted shading: by default, d3.brushX() shades the SELECTED region
  // and leaves the rest clear. We want the opposite — selected/visible
  // region clear, everything outside it grey — so two manual mask rects
  // (left-of-selection, right-of-selection) are drawn and resized on every
  // brush event, and the brush's own default selection fill is hidden.
  // Select-or-create so re-renders reuse the same two rects rather than
  // inserting duplicates each time.
  var maskLeft = brushG.select("rect.brush-mask-left");
  if (maskLeft.empty()) {
    maskLeft = brushG.insert("rect", ":first-child").attr("class", "brush-mask-left");
  }
  maskLeft
    .attr("fill", "#888")
    .attr("fill-opacity", 0.4)
    .attr("y", 0)
    .attr("height", miniChartHeight);

  var maskRight = brushG.select("rect.brush-mask-right");
  if (maskRight.empty()) {
    maskRight = brushG.insert("rect", ":first-child").attr("class", "brush-mask-right");
  }
  maskRight
    .attr("fill", "#888")
    .attr("fill-opacity", 0.4)
    .attr("y", 0)
    .attr("height", miniChartHeight);

  function updateMasks(selection) {
    var x0 = selection[0];
    var x1 = selection[1];

    maskLeft
      .attr("x", 0)
      .attr("width", Math.max(0, x0));

    maskRight
      .attr("x", x1)
      .attr("width", Math.max(0, innerWidth - x1));
  }

  var brush = d3.brushX()
    .extent([[0, 0], [innerWidth, miniChartHeight]])
    .on("brush end", function(event) {
      if (!event.selection) return;
      updateMasks(event.selection);
      var matchupExtent = event.selection.map(xMini.invert);
      updateMainXDomain(matchupExtent);
    });

  brushG.call(brush);

  // Hide the brush's own default selection fill — the masks above now do
  // the visual job of shading, so the built-in .selection rect should be
  // fully transparent (it's still needed, invisibly, for drag interaction).
  brushG.select(".selection")
    .attr("fill", "transparent")
    .attr("stroke", "#666");

  // Apply the persisted/default selection. Using brush.move (rather than
  // just setting x.domain directly) keeps the brush handles' visual
  // position in sync with whatever domain is actually active.
  brushG.call(brush.move, defaultPixelExtent);
  updateMasks(defaultPixelExtent);

  // ======================================================================
  // LEGEND — swatch + label per competitor, in the reserved right
  // margin. Single click toggles that competitor's line on/off (dimmed).
  // Double click isolates it (everyone else dimmed). Double-clicking an
  // already-visible competitor while others are dimmed resets everyone
  // back to visible. All of this acts on a persistent off-set
  // (svgNode.__offSet) that's wiped and replaced whenever the R-driven
  // "Just H2H" option changes (see h2hKey check above), and cleared
  // entirely when H2H turns off.
  // ======================================================================

  legendG.attr("transform", "translate(" + (margin.left + innerWidth + 20) + "," + margin.top + ")");

  var legendItemHeight = 20;

  var legendItems = legendG.selectAll("g.legend-item")
    .data(competitors, function(d) { return d; });

  legendItems.exit().remove();

  var legendEnter = legendItems.enter()
    .append("g")
    .attr("class", "legend-item")
    .style("cursor", "pointer")
    .attr("transform", function(d, i) { return "translate(0," + (i * legendItemHeight) + ")"; });

  legendEnter.append("rect")
    .attr("width", 12)
    .attr("height", 12)
    .attr("rx", 2);

  legendEnter.append("text")
    .attr("x", 18)
    .attr("y", 10)
    .style("font-size", "12px")
    .style("font-family", "sans-serif");

  var legendMerged = legendEnter.merge(legendItems);

  legendMerged.attr("transform", function(d, i) { return "translate(0," + (i * legendItemHeight) + ")"; });

  legendMerged.select("rect")
    .attr("fill", function(d) { return color(d); });

  legendMerged.select("text")
    .text(function(d) { return d; })
    .style("opacity", function(d) { return isOff(d) ? 0.4 : 1; });

  function refreshAllOpacity() {
    pathsMerged.style("opacity", function(p) { return opacityFor(p.name); });
    markersMerged.style("opacity", function(p) { return opacityFor(p.competitor_name); });
    legendMerged.select("text").style("opacity", function(c) { return isOff(c) ? 0.4 : 1; });
  }

  // Distinguish single vs double click: browsers fire a `click` event for
  // each click in a double-click sequence, so a short delay is needed to
  // tell whether a second click follows before acting on the first.
  // Stored on svgNode (not a local var) so an unrelated re-render landing
  // between the two clicks of a double-click doesn't orphan the timer.
  var CLICK_DELAY = 250;

  legendMerged.on("click", function(event, d) {
    if (svgNode.__clickTimer !== null && svgNode.__clickTimer !== undefined) {
      // Second click within the delay window: treat as double-click.
      clearTimeout(svgNode.__clickTimer);
      svgNode.__clickTimer = null;

      var someoneIsDimmed = offSet.length > 0;
      var clickedIsVisible = !isOff(d);

      if (someoneIsDimmed && clickedIsVisible) {
        // d is already visible and at least one other competitor is
        // dimmed: double-clicking a visible entry resets everyone back
        // to visible, rather than isolating d further.
        svgNode.__offSet = [];
      } else {
        // d is currently dimmed (or everyone's already visible): isolate
        // d as usual — off-set becomes everyone except d.
        svgNode.__offSet = competitors.filter(function(c) { return c !== d; });
      }

      offSet = svgNode.__offSet;
      refreshAllOpacity();
    } else {
      // First click: wait to see if a second one follows.
      svgNode.__clickTimer = setTimeout(function() {
        svgNode.__clickTimer = null;

        // Single click: toggle just this competitor in/out of the off-set.
        var idx = offSet.indexOf(d);
        if (idx === -1) {
          svgNode.__offSet = offSet.concat([d]);
        } else {
          svgNode.__offSet = offSet.filter(function(c) { return c !== d; });
        }
        offSet = svgNode.__offSet;
        refreshAllOpacity();
      }, CLICK_DELAY);
    }
  });
});