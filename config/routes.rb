Rails.application.routes.draw do
  root to: 'pages#main'
  
  namespace :api, defaults: { format: :json} do
    get '/transcript', to: 'transcripts#show'
  end
end
