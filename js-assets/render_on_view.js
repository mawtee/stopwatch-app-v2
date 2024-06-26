// Get the element to animate
const element = document.getElementById('hc');
const begin = document.getElementById('begin');

// Define the options for the Intersection Observer
const options = {
  root: null,
  rootMargin: '0px',
  threshold: 0.5
};

// Create a new Intersection Observer
const observer = new IntersectionObserver(function(entries, observer) {
  entries.forEach(entry => {
    // If element is in viewport, add the 'show' class to trigger the animation
    if (entry.isIntersecting) {
     Shiny.setInputValue("render_hc", true, { priority: "event" });
    }
  });
}, options);

// Start observing the element
observer.observe( document.getElementById('begin'));
// https://stackoverflow.com/questions/40208961/using-classlist-when-element-is-set-to-display-none


