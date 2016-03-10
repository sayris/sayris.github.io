function changeColor(newColor) {
    var elem = document.getElementById("para1");
    elem.style.color = newColor;
}

function blueOrRed(){
    var elem = document.getElementById("para1");
    var color=elem.style.color;
    if(color==="red"){
        color="blue";
    } else if(color==="blue") {
        color="black";
    } else {
        color="red";
    }
    
    elem.style.color=color;
}

function imageFwd(type){
    
    
    var elem=document.getElementById(type);
    var source=elem.src;

    var len=source.length;

    var number=Number(source.substring(len-5,len-4));

    if(number===7){
        number=1;
    } else {
        number=number+1;
    }
    
    elem.src="positioned_features/face_"+type+number.toString()+".png";
}


function imageBwd(type){
    var elem=document.getElementById(type);
    var source=elem.src;
    var len=source.length;
    
    var number=Number(source.substring(len-5,len-4));
    
    if(number===1){
        number=7;
    } else {
        number=number-1;
    }
    
    elem.src="positioned_features/face_"+type+number.toString()+".png";
}