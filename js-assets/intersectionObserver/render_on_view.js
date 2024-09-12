// Get the element to animate
const element1 = document.getElementById('natS1-footer');
const element2 = document.getElementById('natS2-footer');
const element3 = document.getElementById('natS2-contents-phase4-trigger');
const element4 = document.getElementById('natS3-trigger')
const element5 = document.getElementById('natS3-footer')
const element6 = document.getElementById('natS4-trigger2')

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

// Create a new Intersection Observer
const observer4 = new IntersectionObserver(function(entries, observer) {
  entries.forEach(entry => {
    // If element is in viewport, add the 'show' class to trigger the animation
    if (entry.isIntersecting) {
     Shiny.setInputValue('render__natS3_cond_panel', true, { priority: "event" });
    }
  });
}, options);

// Create a new Intersection Observer
const observer5 = new IntersectionObserver(function(entries, observer) {
  entries.forEach(entry => {
    // If element is in viewport, add the 'show' class to trigger the animation
    if (entry.isIntersecting) {
     Shiny.setInputValue('render__natS3_ui', true, { priority: "event" });
    }
  });
}, options);

// Create a new Intersection Observer
const observer6 = new IntersectionObserver(function(entries, observer) {
  entries.forEach(entry => {
    // If element is in viewport, add the 'show' class to trigger the animation
    if (entry.isIntersecting) {
     Shiny.setInputValue('render__natS4_cond_panel', true, { priority: "event" });
    }
  });
}, options);



// Start observing the element
observer1.observe( document.getElementById('natS1-footer'));
observer2.observe( document.getElementById('natS2-footer'));
var existCondition1 = setInterval(function() {
  if ($('#natS2-contents-phase4-trigger').length) {
    console.log("Exists!");
    clearInterval(existCondition1);
    observer3.observe( document.getElementById('natS2-contents-phase4-trigger'));
}
}, 100); // check every 100ms
var existCondition2 = setInterval(function() {
  if ($('#natS3-trigger').length) {
    console.log("Exists!");
    clearInterval(existCondition2);
    observer4.observe( document.getElementById('natS3-trigger'));
}
}, 100); // check every 100ms

observer5.observe( document.getElementById('natS3-footer'));
// https://stackoverflow.com/questions/40208961/using-classlist-when-element-is-set-to-display-none

var existCondition3 = setInterval(function() {
  if ($('#natS4-trigger').length) {
    console.log("Exists!");
    clearInterval(existCondition3);
    observer6.observe( document.getElementById('natS4-trigger'));
}
}, 100); // check every 100ms

