import Elm from '../Main'

document.addEventListener('DOMContentLoaded', () => {
  var node = document.getElementById('main');
  var app = Elm.Main.embed(node);

  // Ports

  app.ports.loadVideo.subscribe(function(videoId){
    loadVideo(videoId)
  })

  app.ports.skipToTime.subscribe(function(time){
    player.seekTo(time, true)
  })

  // YouTube Player

  var tag = document.createElement('script');

  tag.src = "https://www.youtube.com/iframe_api";
  var firstScriptTag = document.getElementsByTagName('script')[0];
  firstScriptTag.parentNode.insertBefore(tag, firstScriptTag);

  var player;
  const loadVideo  = function (videoId) {
    player = new YT.Player('player', {
      height: '390',
      width: '640',
      videoId: videoId,
      events: {
        'onReady': onPlayerReady,
        'onStateChange': onPlayerStateChange
      }
    });
  }

  window.onYouTubeIframeAPIReady = function () {
    undefined
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
