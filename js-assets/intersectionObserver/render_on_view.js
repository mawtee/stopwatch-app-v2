// Get the element to animate
const element1 = document.getElementById('natS1-footer');
const element2 = document.getElementById('natS2-contents');
const element3 = document.getElementById('natS2-contents-phase4-trigger');


// Define the options for the Intersection Observer
const options = {
  root: null,
  rootMargin: '0px',
  threshold: 0.5
};

// Create a new Intersection Observer
const observer1 = new IntersectionObserver(function(entries, observer) {
  entries.forEach(entry => {
    // If element is in viewport, add the 'show' class to trigger the animation
    if (entry.isIntersecting) {
     Shiny.setInputValue('render__natS1_ui', true, { priority: "event" });
    }
  });
}, options);

// Create a new Intersection Observer
const observer2 = new IntersectionObserver(function(entries, observer) {
  entries.forEach(entry => {
    // If element is in viewport, add the 'show' class to trigger the animation
    if (entry.isIntersecting) {
     Shiny.setInputValue('render__natS2_ui', true, { priority: "event" });
    }
  });
}, options);

// Create a new Intersection Observer
const observer3 = new IntersectionObserver(function(entries, observer) {
  entries.forEach(entry => {
    // If element is in viewport, add the 'show' class to trigger the animation
    if (entry.isIntersecting) {
     Shiny.setInputValue('render__natS2_phase4_reactVal', true, { priority: "event" });
    }
  });
}, options);




// Start observing the element
observer1.observe( document.getElementById('natS1-footer'));
observer2.observe( document.getElementById('natS2-footer'));
var existCondition = setInterval(function() {
  if ($('#natS2-contents-phase4-trigger').length) {
    console.log("Exists!");
    clearInterval(existCondition);
    observer3.observe( document.getElementById('natS2-contents-phase4-trigger'));
}
}, 100); // check every 100ms

// https://stackoverflow.com/questions/40208961/using-classlist-when-element-is-set-to-display-none


