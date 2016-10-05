document.addEventListener('DOMContentLoaded', function () {
    var talk = {sourceUrl: 'talk.md',
                ratio: '16:9',
                slideNumberFormat: '',
                highlightLanguage: 'no-highlight'};
    window.slideshow = remark.create(talk);
    document.title = slideshow.getSlides()[0].properties.name;
});
