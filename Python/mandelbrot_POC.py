import numpy as np
from PIL import Image
from datetime import datetime
from multiprocessing import Process, Pipe

r = [255.0,255.0,255.0,255.0,255.0,255.0,255.0,255.0,255.0,255.0,255.0,255.0,255.0,255.0,255.0,255.0,255.0,255.0,255.0,255.0,255.0,255.0,255.0,255.0,255.0,255.0,255.0,255.0,255.0,255.0,255.0,255.0,255.0,255.0,255.0,255.0,255.0,255.0,255.0,255.0,255.0,255.0,255.0,255.0,255.0,255.0,255.0,255.0,255.0,255.0,255.0,255.0,255.0,255.0,255.0,255.0,255.0,255.0,255.0,255.0,255.0,255.0,255.0,255.0,255.0,255.0,255.0,255.0,255.0,255.0,255.0,255.0,255.0,255.0,255.0,255.0,255.0,255.0,255.0,255.0,255.0,255.0,255.0,255.0,255.0,255.0,255.0,255.0,255.0,255.0,255.2,252.29999999999998,249.4,246.5,243.6,240.7,237.79999999999998,234.9,232.0,229.1,226.2,223.29999999999998,220.4,217.5,214.6,211.7,208.79999999999998,205.9,203.0,200.1,197.2,194.29999999999998,191.4,188.5,185.6,182.7,179.79999999999998,176.9,174.0,171.1,168.2,165.29999999999998,162.4,159.5,156.6,153.7,150.79999999999998,147.9,145.0,142.1,139.2,136.29999999999998,133.4,130.5,127.6,124.7,121.8,118.89999999999999,116.0,113.1,110.2,107.3,104.39999999999999,101.5,98.6,95.7,92.8,89.89999999999999,87.0,84.1,81.2,78.3,75.39999999999999,72.5,69.6,66.7,63.8,60.9,58.0,55.1,52.199999999999996,49.3,46.4,43.5,40.6,37.699999999999996,34.8,31.9,29.0,26.099999999999998,23.2,20.3,17.4,14.5,11.6,8.7,5.8,2.9,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,2.9,5.8,8.7,11.6,14.5,17.4,20.3,23.2,26.099999999999998,29.0,31.9,34.8,37.699999999999996,40.6,43.5,46.4,49.3,52.199999999999996,55.1,58.0,60.9,63.8,66.7,69.6,72.5,75.39999999999999,78.3,81.2,84.1,87.0,89.89999999999999,92.8,95.7,98.6,101.5,104.39999999999999,107.3,110.2,113.1,116.0,118.89999999999999,121.8,124.7,127.6,130.5,133.4,136.29999999999998,139.2,142.1,145.0,147.9,150.79999999999998,153.7,156.6,159.5,162.4,165.29999999999998,168.2,171.1,174.0,176.9,179.79999999999998,182.7,185.6,188.5,191.4,194.29999999999998,197.2,200.1,203.0,205.9,208.79999999999998,211.7,214.6,217.5,220.4,223.29999999999998,226.2,229.1,232.0,234.9,237.79999999999998,240.7,243.6,246.5,249.4,252.29999999999998,255.2]
g = [0.0,2.9,5.8,8.7,11.6,14.5,17.4,20.3,23.2,26.099999999999998,29.0,31.9,34.8,37.699999999999996,40.6,43.5,46.4,49.3,52.199999999999996,55.1,58.0,60.9,63.8,66.7,69.6,72.5,75.39999999999999,78.3,81.2,84.1,87.0,89.89999999999999,92.8,95.7,98.6,101.5,104.39999999999999,107.3,110.2,113.1,116.0,118.89999999999999,121.8,124.7,127.6,130.5,133.4,136.29999999999998,139.2,142.1,145.0,147.9,150.79999999999998,153.7,156.6,159.5,162.4,165.29999999999998,168.2,171.1,174.0,176.9,179.79999999999998,182.7,185.6,188.5,191.4,194.29999999999998,197.2,200.1,203.0,205.9,208.79999999999998,211.7,214.6,217.5,220.4,223.29999999999998,226.2,229.1,232.0,234.9,237.79999999999998,240.7,243.6,246.5,249.4,252.29999999999998,255.2,255.0,255.0,255.0,255.0,255.0,255.0,255.0,255.0,255.0,255.0,255.0,255.0,255.0,255.0,255.0,255.0,255.0,255.0,255.0,255.0,255.0,255.0,255.0,255.0,255.0,255.0,255.0,255.0,255.0,255.0,255.0,255.0,255.0,255.0,255.0,255.0,255.0,255.0,255.0,255.0,255.0,255.0,255.0,255.0,255.0,255.0,255.0,255.0,255.0,255.0,255.0,255.0,255.0,255.0,255.0,255.0,255.0,255.0,255.0,255.0,255.0,255.0,255.0,255.0,255.0,255.0,255.0,255.0,255.0,255.0,255.0,255.0,255.0,255.0,255.0,255.0,255.0,255.0,255.0,255.0,255.0,255.0,255.0,255.0,255.0,255.0,255.0,255.0,255.0,255.0,255.2,252.29999999999998,249.4,246.5,243.6,240.7,237.79999999999998,234.9,232.0,229.1,226.2,223.29999999999998,220.4,217.5,214.6,211.7,208.79999999999998,205.9,203.0,200.1,197.2,194.29999999999998,191.4,188.5,185.6,182.7,179.79999999999998,176.9,174.0,171.1,168.2,165.29999999999998,162.4,159.5,156.6,153.7,150.79999999999998,147.9,145.0,142.1,139.2,136.29999999999998,133.4,130.5,127.6,124.7,121.8,118.89999999999999,116.0,113.1,110.2,107.3,104.39999999999999,101.5,98.6,95.7,92.8,89.89999999999999,87.0,84.1,81.2,78.3,75.39999999999999,72.5,69.6,66.7,63.8,60.9,58.0,55.1,52.199999999999996,49.3,46.4,43.5,40.6,37.699999999999996,34.8,31.9,29.0,26.099999999999998,23.2,20.3,17.4,14.5,11.6,8.7,5.8,2.9,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0]
b = [0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,2.9,5.8,8.7,11.6,14.5,17.4,20.3,23.2,26.099999999999998,29.0,31.9,34.8,37.699999999999996,40.6,43.5,46.4,49.3,52.199999999999996,55.1,58.0,60.9,63.8,66.7,69.6,72.5,75.39999999999999,78.3,81.2,84.1,87.0,89.89999999999999,92.8,95.7,98.6,101.5,104.39999999999999,107.3,110.2,113.1,116.0,118.89999999999999,121.8,124.7,127.6,130.5,133.4,136.29999999999998,139.2,142.1,145.0,147.9,150.79999999999998,153.7,156.6,159.5,162.4,165.29999999999998,168.2,171.1,174.0,176.9,179.79999999999998,182.7,185.6,188.5,191.4,194.29999999999998,197.2,200.1,203.0,205.9,208.79999999999998,211.7,214.6,217.5,220.4,223.29999999999998,226.2,229.1,232.0,234.9,237.79999999999998,240.7,243.6,246.5,249.4,252.29999999999998,255.2,255.0,255.0,255.0,255.0,255.0,255.0,255.0,255.0,255.0,255.0,255.0,255.0,255.0,255.0,255.0,255.0,255.0,255.0,255.0,255.0,255.0,255.0,255.0,255.0,255.0,255.0,255.0,255.0,255.0,255.0,255.0,255.0,255.0,255.0,255.0,255.0,255.0,255.0,255.0,255.0,255.0,255.0,255.0,255.0,255.0,255.0,255.0,255.0,255.0,255.0,255.0,255.0,255.0,255.0,255.0,255.0,255.0,255.0,255.0,255.0,255.0,255.0,255.0,255.0,255.0,255.0,255.0,255.0,255.0,255.0,255.0,255.0,255.0,255.0,255.0,255.0,255.0,255.0,255.0,255.0,255.0,255.0,255.0,255.0,255.0,255.0,255.0,255.0,255.0,255.0,255.2,252.29999999999998,249.4,246.5,243.6,240.7,237.79999999999998,234.9,232.0,229.1,226.2,223.29999999999998,220.4,217.5,214.6,211.7,208.79999999999998,205.9,203.0,200.1,197.2,194.29999999999998,191.4,188.5,185.6,182.7,179.79999999999998,176.9,174.0,171.1,168.2,165.29999999999998,162.4,159.5,156.6,153.7,150.79999999999998,147.9,145.0,142.1,139.2,136.29999999999998,133.4,130.5,127.6,124.7,121.8,118.89999999999999,116.0,113.1,110.2,107.3,104.39999999999999,101.5,98.6,95.7,92.8,89.89999999999999,87.0,84.1,81.2,78.3,75.39999999999999,72.5,69.6,66.7,63.8,60.9,58.0,55.1,52.199999999999996,49.3,46.4,43.5,40.6,37.699999999999996,34.8,31.9,29.0,26.099999999999998,23.2,20.3,17.4,14.5,11.6,8.7,5.8,2.9,0.0]




def mandelbrotColor(x, y, iterations, hue):

    c = complex(x,y)

    z = 0
    for i in range(1, iterations+1):
        z = z*z + c
        if abs(z) > 4.5: break
    
    if (i == iterations): return [0,0,0]

    peso = (abs(z) - 4.5)/25
    peso = .98 if peso > 1 else peso

    #colorR = iterations/(i+1-peso)/(i/1.7/iterations)

    #colorR = 255 if colorR > 255 else colorR

    #colorR = i%255

    #print(len(r))

    colorR = r[int(iterations/(i+1-peso)*20+hue)%len(r)]
    colorG = g[int(iterations/(i+1-peso)*20+hue)%len(g)]
    colorB = b[int(iterations/(i+1-peso)*20+hue)%len(b)]

    return [colorR, colorG, colorB]

def calcPixel(data, width, height, startingY, endY, size, iterations, offsetX, offsetY, pipe, hue):

    for x in range(int(2*width)):
        for y in range(startingY, endY):
            data[y-startingY][x] = mandelbrotColor((x-width)/size+offsetX, (y-height)/size+offsetY, iterations, hue)
        
    pipe.send((data, startingY, endY))
    pipe.close()

def createMandelbrot(WIDTH, HEIGHT, NUMBERTHREADS, ITERATIONS, SCALE, HUEOFFSET):

    heights = []
    for i in range(NUMBERTHREADS):
        heights.append(
            (int(i*HEIGHT/NUMBERTHREADS),
            int((i+1)*HEIGHT/NUMBERTHREADS))
        )

    threads = []
    for begin, end in heights:
        rcv, snd = Pipe()
        arg = [
            np.zeros((end-begin,WIDTH,3), dtype=np.uint8),
            WIDTH/2,
            HEIGHT/2,
            begin,
            end,
            SCALE,
            ITERATIONS,
            #0.36024044343761436323612524444954530848260780795858575048837581474019534605,
            #-0.64131306106480317486037501517930206657949495228230525955617754306444857417,
            
            #-.8650000261,
            #-.245,

            #-0.7076602118245,
            #0.3527965336373,

            -1.940157358,
            0,

            snd,
            HUEOFFSET
        ]
        process = Process(target=calcPixel, args=arg)
        process.start()
        threads.append((process, rcv))
    
    data = np.zeros((HEIGHT,WIDTH,3), dtype=np.uint8)
    for process, conn in threads:
        res = conn.recv()
        data[res[1]:res[2], 0:WIDTH] = res[0]
        process.join()

    return Image.fromarray(data)

if __name__ == '__main__':

    beginTime = datetime.now()

    NUMBERTHREADS = 16
    WIDTH  = 768
    HEIGHT = 1000

    hueOffset = 35

    scale = 32500#0000
    iter  = int(450)

    img = createMandelbrot(WIDTH, HEIGHT, NUMBERTHREADS, iter, scale, hueOffset)
    img.show()

    img.save(f"./images/mandelbrot.png", "PNG")
    print(f'Computed frame - {NUMBERTHREADS} threads {iter} {scale} - time: {(datetime.now()-beginTime).total_seconds():.1f}s')
