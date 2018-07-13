/* global YT */

import Elm from '../Main'

document.addEventListener('DOMContentLoaded', () => {
  var node = document.getElementById('main')
  var app = Elm.Main.embed(node)

  // Ports

  app.ports.loadVideo.subscribe(function (videoId) {
    loadVideo(videoId)
  })

  app.ports.skipToTime.subscribe(function (time) {
    player.seekTo(time, true)
  })

  // YouTube Player

  var tag = document.createElement('script')

  tag.src = 'https://www.youtube.com/iframe_api'
  var firstScriptTag = document.getElementsByTagName('script')[0]
  firstScriptTag.parentNode.insertBefore(tag, firstScriptTag)

  var player
  const loadVideo = function (videoId) {
    if (player === undefined) {
      player = new YT.Player('player', {
        height: '380px',
        width: '640px',
        videoId: videoId,
        events: {
          'onReady': window.onPlayerReady,
          'onStateChange': window.onPlayerStateChange
        }
      })
    } else {
      player.loadVideoById(videoId)
    }
  }

  window.onYouTubeIframeAPIReady = function () {
    return undefined
  }

  window.onPlayerReady = function (event) {
    event.target.playVideo()
    // setInterval(() => {
    //   console.log(player.getCurrentTime())
    // }, 1000)
  }

  window.onPlayerStateChange = function (event) {
    console.log(event)
  }

  window.stopVideo = function () {
    player.stopVideo()
  }
})
