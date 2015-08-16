function onLoad(){
  loadInputs();
  window.setInterval(loadInputs, 1000);
}

function loadInputs(){
  $("#container").empty();
  jQuery.get("/HSA/execute?name=input-output&msg=get_inputs")
  .done(function(data){
    $p = $(data).find("e").each(function(){
      var inputId = $(this).text();
      $.ajax({
        url: "/HSA/execute?name=input-output&msg=get_input_value",
        data: '<hsd><p n="input" t="l">' + inputId + '</p></hsd>', 
        type: 'POST',
        contentType: "text/xml",
        dataType: "text",
        success : function(data){
          $("<div/>")
          .appendTo("#container")
          .text($(data).find("p").text());
        },
        error : function (xhr, ajaxOptions, thrownError){  
          console.log(xhr.status);
          console.log(thrownError);
        }
      });
    });
  })
  .fail(function(){
    
  });
}