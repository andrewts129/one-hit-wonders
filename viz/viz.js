function getColorForDecade(decade) {
    switch (decade) {
        case "50s":
            return "#e41a1c";
        case "60s":
            return "#377eb8";
        case "70s":
            return "#4daf4a";
        case "80s":
            return "#984ea3";
        case "90s":
            return "#ff7f00";
        case "00s":
            return "#ffff33";
        default:
            return "#000000";
    }
}

function getDecade(year) {
    if (year < 1960) {
        return "50s";
    }
    else if (year < 1970) {
        return "60s";
    }
    else if (year < 1980) {
        return "70s";
    }
    else if (year < 1990) {
        return "80s";
    }
    else if (year < 2000) {
        return "90s";
    }
    else if (year < 2010) {
        return "00s";
    }
    else {
        return "'10s";
    }
}

// Loads in the data from R as an array of objects
$.getJSON("data/OneHitWonders.json", function (response) {
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
    var svg = d3.select("#one-hit-wonder-viz")
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
        .attr("fill", "#222222")
        .attr("text-anchor", "end")
        .style("font-family", "Montserrat, sans-serif")
        .style("font-size", fontSize + "px")
        .style("font-weight", "400")
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
        .attr("fill", "#444444")
        .attr("text-anchor", "end")
        .style("font-family", "Montserrat, sans-serif")
        .style("font-size", fontSize + "px")
        .style("font-weight", "300")
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
        .attr("data-legend", function (d) {
            return getDecade(d.year);
        })
        .attr("fill", function (d) {
            return getColorForDecade(getDecade(d.year));
        });

    // The scores
    svg.selectAll("text.score").data(dataset).enter()
        .append("text")
        .text(function (d) {
            return d.sdRatioScore.toFixed(2)
        })
        .attr("class", "score")
        .attr("x", function (d, i) {
            // For whatever reason the first data point is too far to the right
            if (i === 0) {
                return maxTextLength - 7 + d.sdRatioScore * lengthMultiplier;
            }
            else {
                return maxTextLength - 2 + d.sdRatioScore * lengthMultiplier;
            }
        })
        .attr("y", function (d, i) {
            return i * (barHeight + barPadding) + fontSize + (barHeight / 2);
        })
        .attr("fill", function (d) {
            // White doesn't show well on yellow
            if (getColorForDecade(getDecade(d.year)) === "#ffff33") {
                return "#222222"
            }
            else {
                return "#ffffff"
            }
        })
        .attr("text-anchor", "end")
        .style("font-family", "Montserrat, sans-serif")
        .style("font-size", (fontSize * 1.3) + "px")
        .style("font-weight", "400");
});

