var boxxyBinding = new Shiny.OutputBinding();

$.extend(boxxyBinding, {
  find: function(scope) {
    return $(scope).find(".boxxy");
  },
  renderValue: function(el, data) {

    // insert the title
    let title_id = el.id + '-boxxy-title';
    document.getElementById(title_id).innerText = data.title

    // counter start at 0
    let counter_id = el.id + '-boxxy-value';
    var counter = new CountUp(counter_id, 0, data.value);
    counter.start();

    // background color 
    el.style.backgroundColor = data.color;
  }
});

// register
Shiny.outputBindings.register(boxxyBinding, "john.boxxy");