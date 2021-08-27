ca65 ./PingPong/PingPong.asm -o ./PingPong/PingPong.o -t nes
ld65 ./PingPong/PingPong.o -o ./PingPong/PingPong.nes -t nes
start ./PingPong/PingPong.nes