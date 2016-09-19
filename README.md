# MDS - Emacs

O projeto tem como objetivo ser um modelo de configuração do Emacs para o dia a dia do programador nas seguintes tecnologias: Lisp, Java, C, Web e Escrita (Latex, Markdown, Texto e etc...) - voltado para os amantes em personalização de ferramentas.

Versão do Emacs: 25.1.50

# Instalação

Procedimento para a instalação do projeto.

## Instalação do Emacs

```
git clone https://github.com/emacs-mirror/emacs
./autogen.sh all
./configure
make
sudo make install
```

## Clonagem do Projeto

```
mkdir ~/.emacs.d
cd ~/.emacs.d
git clone https://github.com/mdssjc/mds-emacs
```

# O que fazer?

Durante o desenvolvimento, o projeto servirá de estudo para o conhecimento da plataforma, entretanto recomendo a utilização do Spacemacs como starter kit.

[Spacemacs](https://github.com/syl20bnr/spacemacs)

## Etapas

Primeiramente as configurações básicas, tais como tema - implicitamente as configurações de pacotes e estruturas.

    * Sugestão de Atalhos: Buscar uma opção para facilitar a visualização dos bindings.
    * Autocompletar: instalar Abreviação. Melhorar a configuração dos pacotes Yasnippet.
    * Acompanhar a evolução do Meghanada.

# Projeto

Descrição da composição do projeto.

## Núcleo

    * Cursores: mover/duplicar linha(s), espaçar linha.

## Pacotes

Conjunto de pacotes para o ambiente:

    * Estético: estilo ergonômico e sem distrações/ruídos - tema dark, linha de status com ícones.
    * Sintaxe: conjuntos de funcionalidades para facilitar a codificação e escrita.
      * Autocompletar: sugestão de texto, conforme modo maior.
      * Correção: sugestão para palavras selecionadas, conforme dicionário (português por padrão).
      * Abreviação: X
      * Template: blocos de construção de texto (linguagens de programação).
    * Semântica: frase e/ou expressão incorreta.
      * X
    * Pragmático: org-mode...
      * X
    * Estrutura: conjunto de facilidades ao editor.
      * Documentos recentes
      * Sugestão de atalhos
      * Cliente Git.
      * Pacotes Abo-abo: Avy, Ace-Window, Ivy, Swiper e Counsel
    * Linguagens de Programação:
      * Lisp: dialeto ELisp (Emacs Lisp) e Racket
      * Java: Meghanada (Pacote novo - em teste)
      * C: X
      * Web: X
    * Linguagens de Marcação:
      * Markdown: arquivos .md.
      * XML: X
      * Web: X
      * Latex: X

## Comandos

Listagem dos comandos fundamentais do Emacs:

    * F1 - Ajuda do Emacs (funções, variáveis, atalhos...)
    * F2 - Alterações de coluna
    * F3 - Inicia/Incrementa a gravação de macro
    * F4 - Finaliza/Executa a macro
    * Ctrl + x - configurações e utilitários do ambiente
    * Ctrl + c - comandos para o modo maior atual
    * Alt + x - prompt de comandos
    * Win - comando especial para o modo maior
    * F5 - Toggle (golden-ratio, centered-cursor, ...)
    * F6 - ???
    * F7 - ???
    * F8 - ???
