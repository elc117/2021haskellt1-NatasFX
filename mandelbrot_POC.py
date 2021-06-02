import numpy as np
from PIL import Image
from datetime import datetime
from multiprocessing import Process, Pipe

def mandelbrotColor(x, y, iterations):
    cx = x
    cy = y

    for i in range(1, iterations+1):

        nx = x*x - y*y + cx
        ny = 2*x*y + cy

        y = ny
        x = nx

        if (x*x + y*y > 4): break

    if (i == iterations): return [0,0,0]

    color = iterations/i*30

    color = 255 if color > 255 else color

    return [color, color, color]

def calcPixel(data, width, height, startingY, endY, size, iterations, offsetX, offsetY, pipe):
    
    for x in range(int(2*width)):
        for y in range(startingY, endY):
            data[y-startingY][x] = mandelbrotColor((x-width)/(size)-offsetX, (y-height)/(size)-offsetY, iterations)

    pipe.send((data, startingY, endY))
    pipe.close()

def createMandelbrot(WIDTH, HEIGHT, NUMBERTHREADS, ITERATIONS, SCALE):

    heights = []
    for i in range(NUMBERTHREADS):
        heights.append((i*HEIGHT/NUMBERTHREADS, (i+1)*HEIGHT/NUMBERTHREADS))

    data = np.zeros((HEIGHT,WIDTH,3), dtype=np.uint8)

    threads = []
    for begin, end in heights:
        rcv, snd = Pipe()
        arg = [
            np.zeros((int(end)-int(begin),WIDTH,3), dtype=np.uint8),
            WIDTH/2,
            HEIGHT/2,
            int(begin),
            int(end),
            SCALE,
            ITERATIONS,
            .865000025,
            .245,
            snd
        ]
        process = Process(target=calcPixel, args=arg)
        process.start()
        threads.append((process, rcv))
    
    for process, conn in threads:
        res = conn.recv()
        data[res[1]:res[2], 0:WIDTH] = res[0]
        process.join()

    return Image.fromarray(data)

if __name__ == '__main__':

    elapsed = datetime.now()

    NUMBERTHREADS = 16
    WIDTH  = 4320
    HEIGHT = 7680

    numberOfFrames = 1

    #ffmpeg -framerate 24 -i %d.png -c:v libx264 -pix_fmt yuv420p output.mp4

    scales = []
    for i in range(1, numberOfFrames+1):
        scales.append((i, int((200000/numberOfFrames)*i)))

    for i, scale in scales:
        beginTime = datetime.now()

        img = createMandelbrot(WIDTH, HEIGHT, NUMBERTHREADS, 255, scale)

        img.save(f"./images/{i}.png", "PNG")
        print(f'\rComputed {i+1} frame - {NUMBERTHREADS} threads - time: {(datetime.now()-beginTime).total_seconds():.1f}s', end='')


    print(f'\nRendered {i} frames - time elapsed: {(datetime.now()-elapsed).total_seconds():.0f}s')