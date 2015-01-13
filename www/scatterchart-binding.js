// Put code in an Immediately Invoked Function Expression (IIFE).
// This isn't strictly necessary, but it's good JavaScript hygiene.
(function() {

    // See http://rstudio.github.io/shiny/tutorial/#building-outputs for
    // more information on creating output bindings.

    // First create a generic output binding instance, then overwrite
    // specific methods whose behavior we want to change.
    var binding = new Shiny.OutputBinding();

    binding.find = function(scope) {
        // For the given scope, return the set of elements that belong to
        // this binding.
        return $(scope).find(".nvd3-scatterchart");
    };

    binding.renderValue = function(el, data) {
        // This function will be called every time we receive new output
        // values for a line chart from Shiny. The "el" argument is the
        // div for this particular chart.

        var $el = $(el);

        // The first time we render a value for a particular element, we
        // need to initialize the nvd3 line chart and d3 selection. We'll
        // store these on $el as a data value called "state".
        if (!$el.data("state")) {
//             var chart = nv.models.lineChart()
            var chart = nv.models.scatterChart()
                .margin({left: 100})
//                 .useInteractiveGuideline(true)
//                 .transitionDuration(350)
//                 .showLegend(true)
                .size(1)
                .sizeRange([50,50])
                .showLegend(false)
                .showYAxis(true)
                .showXAxis(true);

            chart.xAxis     //Chart x-axis settings
                .axisLabel('X')
                .tickFormat(d3.format('d'));
//                 .tickFormat(d3.format('.00f'));
//                 .tickFormat(d3.format('.01f'));

                chart.yAxis     //Chart y-axis settings
                .axisLabel('P')
                .tickFormat(d3.format('.02f'));

            nv.utils.windowResize(chart.update);

            var selection = d3.select(el).select("svg");

            // Store the chart object on el so we can get it next time
            $el.data("state", {
                chart: chart,
                selection: selection
            });
        }

        // Now, the code that'll run every time a value is rendered...

        // Retrieve the chart and selection we created earlier
        var state = $el.data("state");

        // Schedule some work with nvd3
        nv.addGraph(function() {
            // Update the chart
            state.selection
                .datum(data)
                .transition(500)
                .call(state.chart);
            return state.chart;
        });
    };

    // Tell Shiny about our new output binding
    Shiny.outputBindings.register(binding, "stat_dist.nvd3-scatterchart");

})();
