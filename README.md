# MDS - Emacs

O projeto tem como objetivo ser um modelo de configuração do Emacs para o dia a dia do programador nas seguintes tecnologias: Lisp, Java, C, Web e Escrita (Latex, Markdown, Texto e etc...) - voltado para os amantes em personalização de ferramentas.

Versão do Emacs: 25.1.50

# Instalação

Instalação do Emacs

Clonagem do Projeto

# O que fazer?

Durante o desenvolvimento, o projeto servirá de estudo para o conhecimento da plataforma, entretanto recomendo a utilização do Spacemacs como starter kit.

[Spacemacs](https://github.com/syl20bnr/spacemacs)

## Etapas

Primeiramente as configurações básicas, tais como tema - implicitamente as configurações de pacotes e estruturas.

    * Tema: instalado - falta melhorar os icones, personalizar o powerline.
    * Sugestão de Atalhos: disponível o Which-key - apresenta automaticamente os atalhos no minibuffer, conforme utilização. Buscar uma opção para facilitar a visualização dos bindings.
    * Autocompletar: disponível o Company, falta o Yasnippet, *Ispell* e Abreviação. Focar no Ispell.

# Projeto

Descrição da composição do projeto.

## Núcleo

    * Cursores: mover/duplicar linha(s), espaçar linha.

## Pacotes

    * Estético: estilo ergonômico e sem distrações/ruídos - tema dark, linha de status com icones.
    * Sintaxe: conjuntos de funcionalidades para facilitar a codificação e escrita.
      * Autocompletar: Company
      * Correção: X
      * Abreviação: X
      * Template: X
    * Semântica: frase e/ou expressão incorreta.
    * Pragmático: org-mode...
    * Estrutura: conjunto de facilidades.
      * Sugestão de Atalhos: Which-key
      * *Git: manipulação das funcionalidades do git.*
      * Pacotes Abo-abo: Avy, Ace-Window, Ivy, Swiper e Counsel
    * Linguagens de Programação:
      * Lisp: dialeto ELisp (Emacs Lisp)
        * Lispy
        * Rainbow-delimiters
      * C, Java e Web.
    * Linguagens de Marcação:
      * Markdown
      * XML: ...
      * Web: ...
      * Latex: ...

## Comandos

Listagem dos comandos fundamentais do Emacs:

    * F1 - Ajuda do Emacs (funções, variáveis, atalhos...)
    * F2 - Alterações de coluna
    * F3 - Inicia/Incrementa a gravação de macro
    * F4 - Finaliza/Executa a macro
    * ?? - Comando Principal
