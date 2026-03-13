$(document).on("click", "a.word-html-link", function (e) {
  e.preventDefault();

  const payload = $(this).data("link") || $(this).text().trim();

  if (window.Shiny && typeof Shiny.setInputValue === "function") {
    Shiny.setInputValue("word_html_link", payload, { priority: "event" });
  }
});
