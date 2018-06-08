import Elm from '../Main'

document.addEventListener('DOMContentLoaded', () => {
  const app = document.createElement('div')

  document.body.appendChild(app)
  Elm.Main.embed(app)
})

var tag = document.createElement('script');

tag.src = "https://www.youtube.com/iframe_api";
var firstScriptTag = document.getElementsByTagName('script')[0];
firstScriptTag.parentNode.insertBefore(tag, firstScriptTag);

// 3. This function creates an <iframe> (and YouTube player)
//    after the API code downloads.
var player;
window.onYouTubeIframeAPIReady = function () {
  player = new YT.Player('player', {
    height: '390',
    width: '640',
    videoId: 'M7lc1UVf-VE',
    events: {
      'onReady': onPlayerReady,
      'onStateChange': onPlayerStateChange
    }
  });
}

window.onPlayerReady = function (event) {
  event.target.playVideo();
}

var done = false;
window.onPlayerStateChange = function (event) {
 
}
window.stopVideo = function () {
  player.stopVideo();
}