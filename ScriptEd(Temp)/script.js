//Functions
function red(){
    $("body").css("background-color","red");
    $("body").css("color","white");
    $("img").attr("src", "https://images-mm.s3.amazonaws.com/Futurama_Bender_Face_Gray_Shirt_POP.jpg");
    $("#result").html("");
}

function blue(){
    $("body").css("background-color","blue");
    $("body").css("color","white");
    $("img").attr("src", "https://images-mm.s3.amazonaws.com/Futurama_Bender_Face_Gray_Shirt_POP.jpg");
    $("#result").html("");
}

function bot1(){
    $("img").attr("src", "https://upload.wikimedia.org/wikipedia/commons/b/bd/Pink_or_Plum_Robot_Face_With_Green_Eyes.png");
    $("body").css("background-color","#f9bbec");
    $("body").css("color","black");
    $("#result").html("Hi, I'm Tina.");
}

function bot2(){
    $("img").attr("src", "https://ih1.redbubble.net/image.195026479.5586/sticker,375x360-bg,ffffff.u3.png");
    $("body").css("background-color","black");
    $("body").css("color","white");
    $("#result").html("Hi, I'm Toby.");
}

//Click Handlers
$("#red").click(function(){
    red(); 
});

$("#blue").click(function(){
    blue(); 
});

$("#bot1").click(function(){
    bot1(); 
});

$("#bot2").click(function(){
    bot2(); 
});

function doThing() {
    console.log('why aren\'t you working??!?!?!');
    document.getElementById("blue").addEventListener("click", ()=>{console.log('asfd');})
}