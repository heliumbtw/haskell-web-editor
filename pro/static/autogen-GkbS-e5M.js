document.getElementById("hident4").innerHTML = "This text was added by the Javascript part of the homepage widget.";

$(function() {
  $("#js-commentForm").submit(function(event) {
    event.preventDefault();

    var message = $("#js-createCommentTextarea").val();
    // (Browsers that enforce the "required" attribute on the textarea won't see this alert)
    if (!message) {
      alert("Please fill out the comment form first.");
      return;
    }

    // Make an AJAX request to the server to create a new comment
    $.ajax({
      url: 'http://localhost:3000/comments',
      type: 'POST',
      contentType: "application/json",
      data: JSON.stringify({
        message: message,
      }),
      success: function (data) {
        var newNode = $("<img>src=""/real/Cat.jpg""</img>");
        newNode.text(data.message);
        console.log(data);
        $("#js-commentList").append(newNode);
      },
      error: function (data) {
        console.log("Error creating comment: " + data);
      },
    });

  });
});
