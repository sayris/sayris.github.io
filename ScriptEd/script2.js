function Window(color, numRows, numCols, xSide, yBottom, yTop, xGap, yGap) {
	this.color = color;
	this.numRows = numRows;
	this.numCols = numCols;
	this.xGap = xGap;
	this.yGap = yGap;
	this.yBottom = yBottom;
	this.yTop = yTop;
	this.xSide = xSide;
}

function Building(color, x, bldWidth, bldHeight, window) {
	this.color = color;
	this.x = x;
	this.y = calcYForGround(bldHeight);
	this.width = bldWidth;
	this.height = bldHeight;
	this.window = window;

	// calculates the width of a window based on 
	// the passed in window object
	this.wndW = (this.width - 
				(2 * this.window.xSide) - 
			   ((this.window.numCols - 1) * this.window.xGap)) /
			     this.window.numCols;

	// calculates the height of a window based on 
	// the passed in window object
	this.wndH = (this.height -
				 this.window.yBottom - this.window.yTop -
			   ((this.window.numRows - 1) * this.window.yGap)) / 
				 this.window.numRows;

	this.drawWindows = () => {
		for (let i = 0; i < this.window.numCols; i++){
			const wndXCoord = this.x + 
							  this.window.xSide + 
						      i * (this.wndW + this.window.xGap);
			for(let j = 0; j < this.window.numRows; j++){
				const wndYCoord = this.y + 
								  this.window.yTop + 
								  j * (this.wndH + this.window.yGap);

				drawRectangle(wndXCoord,
							  wndYCoord,
							  this.wndW,
							  this.wndH,
							  this.window.color);
			}
		}
	}

	this.drawBuilding = () => {
		drawRectangle(this.x, this.y, this.width, this.height, this.color);
	};

	this.draw = () => {
		this.drawBuilding();
		this.drawWindows();
	};
}

function building1(x, width, height, color, windowColor){
	const windows = 
		new Window(windowColor,
				   5,  // number of window rows
				   2,  // number of window columns
				   13, // amount of space between side of window and buidling
				   30, // y offset for the bottom
				   10,  // y offset from the top
				   15,  // gaps between window columns 
				   5); // gaps between window rows
	const building =
		new Building(color, x, width, height, windows);

	building.draw();
}

function building2(x, width, height, color, windowColor) {
	const windows = 
		new Window(windowColor,
				   5,  // number of window rows
				   3,  // number of window columns
				   0, // amount of space between side of window and buidling
				   10, // y offset for the bottom
				   8,  // y offset from the top
				   4,  // gaps between window columns 
				   4); // gaps between window rows
	const building =
		new Building(color, x, width, height, windows);
	building.draw();
}

function building3(x, width, height, color, windowColor){
	const windows = 
		new Window(windowColor,
				   3,  // number of window rows
				   4,  // number of window columns
				   5, // amount of space between side of window and buidling
				   50, // y offset for the bottom
				   10,  // y offset from the top
				   2,  // gaps between window columns 
				   5); // gaps between window rows
	const building =
		new Building(color, x, width, height, windows);

	building.draw();
}


function building1_smallWindows(x, buildingColor, windowColor){
	var width = 60;
	var height = 100;
	var y = canvas.height - height;

	var windowWidth = 10;
	var wGap = 8;
	var windowHeight = 25;
	var hGap = 10;
	var totalWindowW = 2 * wGap + 3 * windowWidth;
	var endGap = (width - totalWindowW)/2;
	windowWidth = ((endgap * 2) - bldWidth + ((numWindow - 1) * wGap)) / numWindow  ;

	drawRectangle(x, y, width, height, buildingColor);

	for(let xW = 0; xW < 3; xW++){
		const xCoord = x + endGap + xW * (windowWidth + wGap);
		for(let yW = 0; yW < 2; yW++){
			const yCoord = y + 5 + yW * (windowHeight + hGap);
			drawRectangle(xCoord, yCoord, windowWidth, windowHeight, windowColor);
		}
	}
}

function triangleBuilding(x, width, height, buildingColor, windowColor) {
	const window = new Window(windowColor,
				   12,  // number of window rows
				   2,  // number of window columns
				   8, // amount of space between side of window and buidling
				   5, // y offset for the bottom
				   10,  // y offset from the top
				   7,  // gaps between window columns 
				   4); // gaps between window rows
	const building = 	
		new Building(buildingColor, x, width, height, window);
	const roofHeight = 25;
	const roofY = building.y - roofHeight;
	building.draw();
	drawTriangle(
				 x, roofY,
				 x, building.y,
				 x + width, building.y,
				 buildingColor);
}


$("#button1").click(function(){
	reset();
	triangleBuilding(200, 50, 200, "#FFF", "#B7E2DB");
	building1(240, 60, 120, "#E9BA32", "#FFFFFD")
	triangleBuilding(170, 50, 160, "#FFF", "#B7E2DB");
	building2(55, 70, 190, "#FFF", "#A3A0AC");
	building1(10, 60, 120, "#E48166", "#FEFDFF");
});

const white = "#FFF";
const pink = "#E48166"
const whiteWindow = "#FEFDFF";
const blueWindow = "#B7E2DB";
const purple = "#685266";
const yellow = "#E9BA32";
const purpleWindow = "#A3A0AC";
const darkGrey = "#8A887F";


$("#button2").click(function() {
	reset();
	triangleBuilding(450, 60, 250, pink, whiteWindow);
	triangleBuilding(480, 60, 210, yellow, whiteWindow);
	building2(280, 70, 190, purpleWindow, white);
	building3(390, 100, 140, blueWindow, white);
	building3(320, 100, 80, darkGrey, white);
	building2(60, 160, 190, purple, white);
	building1(210, 60, 160, pink, whiteWindow);
	building1(175, 60, 120, yellow, whiteWindow);
	triangleBuilding(0, 200, 50, purpleWindow, white);
});