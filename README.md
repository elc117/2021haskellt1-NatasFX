Essa branch é uma versão que tentei fazer em gloss do Mandelbrot.

Utilizei gloss-raster e paralelizei mas ainda assim ficam com FPS muito baixo.

Então vou mudar o escopo da aplicação.

Incluido tem o comando que usei na minha maquina pra rodar, extraído do `cabal new-run -v`

No main, aumentar o valor da tupla (1,1) aumentará o fps ao custo da resolução. O calculo do mandelbrot leva muito menos tempo que a geração das imagem a partir da matriz.

A função render é responsavel por ler a matriz e devolver uma cor em determinado ponto. A função provavelmente é rápida, entretanto tem algo dentro do gloss-raster que está demorando muito.

Eu utilizava uma lib matrix para atualizar o mandelbrot quando for dar zoom mas ela estava vazando memória, então fiz minha matrix mesmo.

