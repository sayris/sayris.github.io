const canvas = document.getElementById("canvas");
const ctx = canvas.getContext('2d');

function reset() {
	ctx.clearRect(0, 0, canvas.width, canvas.height);
}

function drawRectangle(x, y, width, height, color) {
	ctx.fillStyle = color;
	ctx.fillRect(x, y, width, height);
}

function drawCircle(x, y, radius, color) {
	ctx.arc(x, y, radius, 0,2*Math.PI);
	ctx.fill(color);
}

function drawTriangle(x1, y1, x2, y2, x3, y3, color) {
	ctx.fillStyle = color;
	ctx.beginPath();
	ctx.moveTo(x1, y1);
	ctx.lineTo(x2, y2);
	ctx.lineTo(x3, y3);
	ctx.fill();
}

function calcYForGround(height) {
	return canvas.height - height;
}