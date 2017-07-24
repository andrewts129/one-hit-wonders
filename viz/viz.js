function getColorForDecade(year) {
    if (year < 1960) {
        return "#FF0000";
    }
    else if (year < 1970) {
        return "#FFFF00";
    }
    else if (year < 1980) {
        return "#00FF00";
    }
    else if (year < 1990) {
        return "#00FFFF";
    }
    else if (year < 2000) {
        return "#0000FF";
    }
    else if (year < 2010) {
        return "#FF00FF";
    }
    else {
        return "#800080";
    }
}

// Loads in the data from R as an array of objects
$.getJSON("OneHitWonders.json", function (response) {
    var dataset = response;

    var fontSize = 16;

    // The height of each data bar, and the pixels of space between them
    var barHeight = fontSize * 2 + 15;
    var barPadding = barHeight / 16;

    // The canvas should be 90% of the page's width
    var canvasWidth = $(window).width() * 0.9;

    // Gets the height of the canvas according to the height and number of bars
    var canvasHeight = (barHeight + barPadding) * dataset.length;

    // The canvas
    var svg = d3.select("body")
        .append("svg")
        .attr("width", canvasWidth)
        .attr("height", canvasHeight);

    // This will be set to the maximum length, in pixels, of the labels
    var maxTextLength = 0;

    // The artist names
    svg.selectAll("text.artist").data(dataset).enter()
        .append("text")
        .text(function (d) {
            return (d.artists);
        })
        .attr("class", "artist")
        .attr("x", 0)
        .attr("y", function (d, i) {
            return i * (barHeight + barPadding) + (barHeight / 20) + fontSize;
        })
        .attr("fill", "black")
        .attr("text-anchor", "end")
        .style("font-size", fontSize + "px")
        .each(function () {
            var textWidth = this.getComputedTextLength();
            if (textWidth > maxTextLength) {
                maxTextLength = textWidth;
            }
        });

    // The song names
    svg.selectAll("text.song").data(dataset).enter()
        .append("text")
        .text(function (d) {
            return d.hit + " - " + d.year;
        })
        .attr("class", "song")
        .attr("x", 0)
        .attr("y", function (d, i) {
            return i * (barHeight + barPadding) + (barHeight / 25 ) + fontSize + (barHeight / 10) + fontSize;
        })
        .attr("fill", "black")
        .attr("text-anchor", "end")
        .style("font-size", fontSize + "px")
        .each(function () {
            var textWidth = this.getComputedTextLength();
            if (textWidth > maxTextLength) {
                maxTextLength = textWidth;
            }
        });

    // Right align the text
    svg.selectAll("text.artist").attr("x", maxTextLength);
    svg.selectAll("text.song").attr("x", maxTextLength);

    // The longest bar should be the length of the canvas
    var lengthMultiplier = (canvasWidth - maxTextLength) / dataset[0].sdRatioScore;

    // The bars
    svg.selectAll("rect").data(dataset).enter()
        .append("rect")
        .attr("x", maxTextLength + 5)
        .attr("y", function (d, i) {
            return i * (barHeight + barPadding);
        })
        .attr("width", function (d) {
            return d.sdRatioScore * lengthMultiplier;
        })
        .attr("height", barHeight)
        .attr("fill", function (d) {
            return getColorForDecade(d.year);
        });

});

