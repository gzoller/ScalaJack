VAR1="$(docker run -p27017:27017 -d mongo)"
sleep 5 
docker exec -it "${VAR1}" mongo
