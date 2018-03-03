
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

	drawRectangle(x, y, width, height, buildingColor);

	for(let xW = 0; xW < 3; xW++){
		const xCoord = x + endGap + xW * (windowWidth + wGap);
		for(let yW = 0; yW < 2; yW++){
			const yCoord = y + 5 + yW * (windowHeight + hGap);
			drawRectangle(xCoord, yCoord, windowWidth, windowHeight, windowColor);
		}
	}
}

function building2(x, height, width, buildingColor, windowColor) {
	var hRoof = 25;
	var y = canvas.height - height;
	var yRoof = canvas.height - height - 35;
	drawRectangle(x, y, width, height, buildingColor);
	drawTriangle(x, y, x + width, yRoof, x + width, y, buildingColor);
}