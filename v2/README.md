# Ideia
Inicialmente, fiquei em dúvida do que fazer. Depois de muito pensar, de repente visualizo minha mãe jogando um joguinho em seu celular. Então tive uma ideia: lembrei que ela gostava muito de jogar um jogo de celular chamado "Cody Cross". Com isso, decidi implementar um jogo de Acróstico Cruzado. É um jogo divertido, o qual eu gosto de jogar e que pode ser jogado por mais pessoas aqui de minha casa. A lógica do jogo é a seguinte: há algumas palavras na horizontal para serem respondidas com base em dicas, e em cada palavra horizontal a primeira letra já está inicialmente preenchida. Essas letras formam uma palavra na vertical.

# Processo de criação do jogo
A seguir, descrevo como foi o processo de criação do jogo, as dificuldades encontradas pelo caminho e algumas outras informações.

## 1.
Para uma primeira versão, decidi criar a estrutura que armazena a palavra vertical e as palavras horizontais. Nesta versão, para ir testando, as palavras eram fixas e ainda estavam "inteiras", sem considerar cada letra individualmente. Tive que converter para o formato de JSON, para se adequar e fazer as requisições. Testei por meio de GET, para ver se o servidor Scooty conseguia pegar as palavras e entregá-las formatadas. Funcionou, sem maiores dificuldades.

## 2.
Tendo feito o passo inicial, estava pronto para ir à próxima fase: decompor cada palavra letra a letra, para que pudessem ser identificadas cada uma das letras presentes na palavra individualmente. Para isso, precisei criar mais uma estrutura. Essa estrutura guarda o estado atual do jogo, onde cada uma das letras pode ser preenchida individualmente.