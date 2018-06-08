import Elm from '../Main'

document.addEventListener('DOMContentLoaded', () => {
  var node = document.getElementById('main');
  var app = Elm.Main.embed(node);

  // Ports

  app.ports.loadVideo.subscribe(function(videoId){
    console.log(videoId)
  })

  // YouTube Player

  var tag = document.createElement('script');

  tag.src = "https://www.youtube.com/iframe_api";
  var firstScriptTag = document.getElementsByTagName('script')[0];
  firstScriptTag.parentNode.insertBefore(tag, firstScriptTag);

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
})
