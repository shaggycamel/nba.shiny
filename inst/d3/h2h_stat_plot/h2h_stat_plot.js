// stackedbar.js
// r2d3 script — 100% stacked bar chart (ggplot geom_col(position = "fill")
// equivalent), with:
//   - d3.stackOffsetExpand to normalize each x-category to 0-1
//   - fixed 50% reference line
//   - reversed Set2 palette (scale_fill_brewer("Set2", direction = -1))
//   - hovermode="x"-style tooltip: one tooltip per x-category, listing
//     every competitor's pre-formatted label string together
//
// Expected input data shape (one row per name x competitor, long format —
// same shape ggplot/geom_col consumed):
//   [{ name: "...", competitor: "...", value: 0.42, label: "..." }, ...]
//
// options:
//   competitors : array of competitor names, in desired stack/legend order
//                 (top of stack = last in array, matching ggplot's default
//                 stacking order). If omitted, inferred from data in
//                 first-seen order.
//   height, width, margin : standard sizing overrides

// ---- persistent setup (created once, reused across re-renders) --------

var margin = options.margin || { top: 20, right: 20, bottom: 30, left: 20 };

var g = svg.select("g.plot-area");
if (g.empty()) {
  svg.style("overflow", "hidden");
  g = svg.append("g").attr("class", "plot-area");
  svg.append("g").attr("class", "x-axis");
  svg.append("line").attr("class", "ref-line")
    .attr("stroke", "#333")
    .attr("stroke-width", 1);
  svg.append("g").attr("class", "competitor-labels");
}

var xAxisG = svg.select(".x-axis");
var refLine = svg.select(".ref-line");
var labelsG = svg.select(".competitor-labels");

// Tooltip div — keyed to this chart's own <svg> DOM node (via a property,
// not a random id) so re-renders reuse the same div instead of creating a
// new one each time. r2d3 re-runs this whole script on every renderD3()
// call, so anything keyed by Math.random() would otherwise leak a fresh
// div into <body> on every single data update.
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

r2d3.onRender(function(data, svg, width, height, options) {
  // ---- reshape long data into wide (one row per `name`, one column per competitor) ----

  var names = Array.from(new Set(data.map(function(d) { return d.name; })));
  var inferredCompetitors = Array.from(new Set(data.map(function(d) { return d.competitor; })));
  var competitors = (Array.isArray(options.competitors) && options.competitors.length > 0)
    ? options.competitors
    : inferredCompetitors;

  var innerWidth = width - margin.left - (margin.right || 20);
  var innerHeight = height - margin.top - margin.bottom;

  g.attr("transform", "translate(" + margin.left + "," + margin.top + ")");

  var byName = names.map(function(nm) {
    var row = { name: nm };
    var labels = {};
    data.filter(function(d) { return d.name === nm; }).forEach(function(d) {
      row[d.competitor] = d.value;
      labels[d.competitor] = d.label;
    });
    row.__labels = labels;
    return row;
  });

  var stackGen = d3.stack()
    .keys(competitors)
    .offset(d3.stackOffsetExpand); // normalizes each x-category to sum to 1, matches position="fill"

  var series = stackGen(byName);

  // ---- scales ----

  var x = d3.scaleBand()
    .domain(names)
    .range([0, innerWidth])
    .padding(0.2);

  var y = d3.scaleLinear()
    .domain([0, 1])
    .range([innerHeight, 0]);

  var color = d3.scaleOrdinal()
    .domain(competitors)
    .range(d3.schemeSet2.slice(0, competitors.length).reverse());

  // ---- axis (x only — labs(y=NULL) means no y-axis drawn, matching theme) ----

  xAxisG
    .attr("transform", "translate(" + margin.left + "," + (margin.top + innerHeight) + ")")
    .transition().duration(300)
    .call(d3.axisBottom(x).tickSizeOuter(0));

  // ---- 50% reference line ----

  refLine
    .attr("x1", margin.left)
    .attr("x2", margin.left + innerWidth)
    .transition().duration(300)
    .attr("y1", margin.top + y(0.5))
    .attr("y2", margin.top + y(0.5));

  // ---- competitor name labels: two white boxes pinned to fixed y
  // positions (10% and 90% of the 0-1 stack scale), horizontally centered
  // across the chart width. Top box = last competitor in stack order
  // (topmost segment), bottom box = first competitor in stack order
  // (bottommost segment). ----

  var topCompetitor = competitors[competitors.length - 1];
  var bottomCompetitor = competitors[0];

  var labelBoxData = [
    { competitor: topCompetitor, yFrac: 0.90, anchor: "top" },    // y=90% on 0-1 scale = near top of chart (y is inverted)
    { competitor: bottomCompetitor, yFrac: 0.10, anchor: "bottom" } // y=10% on 0-1 scale = near bottom of chart
  ];

  var labelFontSize = 19;
  var labelBoxPadding = 8;
  var labelBoxHeight = 34;

  labelsG.attr("transform", "translate(" + margin.left + "," + margin.top + ")");

  var labelItems = labelsG.selectAll("g.competitor-label")
    .data(labelBoxData, function(d) { return d.anchor; });

  var labelEnter = labelItems.enter()
    .append("g")
    .attr("class", "competitor-label");

  labelEnter.append("rect")
    .attr("class", "label-box-bg")
    .attr("fill", "white")
    .attr("stroke", "#ccc")
    .attr("stroke-width", 1)
    .attr("rx", 4)
    .attr("height", labelBoxHeight);

  labelEnter.append("text")
    .attr("class", "label-box-text")
    .style("font-size", labelFontSize + "px")
    .style("font-family", "sans-serif")
    .attr("text-anchor", "middle")
    .attr("dominant-baseline", "middle");

  var labelMerged = labelEnter.merge(labelItems);

  // Text sets first (centered on chart midpoint), so its measured width
  // can size and center the background rect under it.
  labelMerged.select("text.label-box-text")
    .text(function(d) { return d.competitor; })
    .attr("x", innerWidth / 2)
    .attr("y", function(d) { return y(d.yFrac); });

  var labelMaxBoxWidth = Math.max(innerWidth - 8, 0); // 4px breathing room on each side

  labelMerged.select("rect.label-box-bg")
    .attr("y", function(d) { return y(d.yFrac) - labelBoxHeight / 2; })
    .attr("width", function(d) {
      // Measure the sibling text node just set above to size the box snugly,
      // but never exceed the chart's own drawable width — an overlong name
      // shouldn't be able to push the SVG's bounds outward and trigger a
      // scrollbar on the containing card.
      var textNode = d3.select(this.parentNode).select("text.label-box-text").node();
      var naturalWidth = textNode.getBBox().width + labelBoxPadding * 2;
      return Math.min(naturalWidth, labelMaxBoxWidth);
    })
    .attr("x", function(d) {
      var textNode = d3.select(this.parentNode).select("text.label-box-text").node();
      var naturalWidth = textNode.getBBox().width + labelBoxPadding * 2;
      var boxWidth = Math.min(naturalWidth, labelMaxBoxWidth);
      var idealX = innerWidth / 2 - boxWidth / 2;
      // Clamp so the box itself never sits outside [0, innerWidth].
      return Math.max(0, Math.min(idealX, innerWidth - boxWidth));
    });

  // Note: if a competitor name is long enough that its natural width
  // exceeds labelMaxBoxWidth, the box clamps to the available width but
  // the text itself may visually extend slightly past the box edge.
  // That's a cosmetic edge case — the box never exceeds the chart's
  // bounds, which is what was causing the page-level scrollbar.

  // Text must render on top of its own box, but boxes are appended before
  // text within each <g> — raise text after sizing so it isn't hidden.
  labelMerged.select("text.label-box-text").raise();

  // ---- stacked bars: one <g> per competitor layer, rects keyed by name ----

  var layers = g.selectAll("g.layer")
    .data(series, function(d) { return d.key; });

  layers.exit().remove();

  var layersEnter = layers.enter()
    .append("g")
    .attr("class", "layer");

  var layersMerged = layersEnter.merge(layers);

  var rects = layersMerged.selectAll("rect")
    .data(
      function(d) { return d.map(function(segment) { return { segment: segment, key: d.key }; }); },
      function(d) { return d.segment.data.name; }
    );

  rects.exit()
    .transition().duration(200)
    .attr("height", 0)
    .remove();

  rects.transition().duration(300)
    .attr("fill", function(d) { return color(d.key); })
    .attr("x", function(d) { return x(d.segment.data.name); })
    .attr("width", x.bandwidth())
    .attr("y", function(d) { return y(d.segment[1]); })
    .attr("height", function(d) { return y(d.segment[0]) - y(d.segment[1]); });

  rects.enter()
    .append("rect")
    .attr("fill", function(d) { return color(d.key); })
    .attr("x", function(d) { return x(d.segment.data.name); })
    .attr("width", x.bandwidth())
    .attr("y", innerHeight)
    .attr("height", 0)
    .transition().duration(300)
    .attr("y", function(d) { return y(d.segment[1]); })
    .attr("height", function(d) { return y(d.segment[0]) - y(d.segment[1]); });

  // ---- per-segment tooltip: hover a single stacked segment to see only
  // that competitor's own label, not the whole x-category's group.
  // Tooltip anchors below the cursor (grows downward) when hovering the
  // top segment of the stack, and above the cursor (grows upward) when
  // hovering the bottom segment. ----

  var hoveredIsTop = false;
  var measuredWidth = 0;
  var measuredHeight = 0;

  layersMerged.selectAll("rect")
    .on("mouseover", function(event, d) {
      var label = d.segment.data.__labels[d.key];
      if (label == null) return;

      // Top segment's upper edge sits at (or essentially at) 1 on the
      // normalized 0-1 stack; bottom segment's lower edge sits at 0.
      hoveredIsTop = d.segment[1] >= 0.999;

      var html = label.replace(/\n/g, "<br>");

      // Measure natural size first: temporarily set content + auto sizing
      // with opacity 0 (invisible, no transition) so offsetWidth/Height
      // reflect the final fully-grown size, not a mid-transition value.
      tooltip
        .style("transition", "none")
        .style("width", "auto")
        .style("height", "auto")
        .style("opacity", 0)
        .html(html);

      measuredWidth = tooltip.node().offsetWidth;
      measuredHeight = tooltip.node().offsetHeight;

      // Snap back to 0x0 (still untransitioned), then re-enable transitions
      // and grow to the measured size — this is the actual animation.
      tooltip
        .style("width", "0px")
        .style("height", "0px");

      // Force layout flush so the browser registers the 0x0 state before
      // we re-enable transitions and change size again on the next tick.
      tooltip.node().offsetHeight; // eslint-disable-line no-unused-expressions

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
      var topPos;

      if (hoveredIsTop) {
        // Cascade down: tooltip's top edge starts just below the cursor.
        topPos = event.pageY + 12;
      } else {
        // Cascade up: tooltip's bottom edge ends just above the cursor.
        // Uses the measured (final) height, not a live mid-animation read.
        topPos = event.pageY - 12 - measuredHeight;
      }

      // Clamp so the tooltip's box never extends past the viewport edges —
      // an unclamped position here is what was pushing the document's
      // bounds outward and triggering a page-level scrollbar.
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
});

r2d3.onResize(function(width, height) {
  r2d3.svg.dispatch("render");
});