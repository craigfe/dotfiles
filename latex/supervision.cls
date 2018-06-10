\ProvidesClass{supervision}
\LoadClass[a4paper]{article}

\RequirePackage[a4paper,margin=1in]{geometry} % Override default (awful) margins
\RequirePackage[titletoc,title]{appendix}
\RequirePackage{listings}
\RequirePackage[dvipsnames]{xcolor} % Better colour support
\RequirePackage{enumerate}
\RequirePackage{color}
\RequirePackage{graphicx}
\RequirePackage{tabularx}
\RequirePackage{float}
\RequirePackage{tabto}
\RequirePackage{svg}
\RequirePackage{fancyhdr}            % Headers and footers
\RequirePackage{lastpage}            % Get a pagecount for the footer
\RequirePackage{etoolbox}
\RequirePackage{mathtools}
\RequirePackage{xparse}
\RequirePackage{microtype}           % Slightly better typesetting, for nerds
\RequirePackage{scrextend}           % addmargin environment

\renewcommand*{\ttdefault}{cmtt} % Restore tt font to default

\newcommand{\Course}{<no course>}
\newcommand{\SupervisionNumber}{0}
\newcommand{\Title}{\Course\ - Supervision \SupervisionNumber}
\newcommand{\Date}{\today}
\newcommand{\Me}{Craig Ferguson}
\newcommand{\CRSID}{cf443}
\newcommand{\College}{Churchill College}

% Set default Java colouring -- similar to Eclipse IDE
\definecolor{javared}{rgb}{0.6,0,0} % for strings
\definecolor{javagreen}{rgb}{0.25,0.5,0.35} % comments
\definecolor{javapurple}{rgb}{0.5,0,0.35} % keywords
\definecolor{javadocblue}{rgb}{0.25,0.35,0.75} % javadoc

% Default settings for lstlisting -- not that great
\lstset{language=Java,
basicstyle=\fontfamily{zi4}\selectfont,
keywordstyle=\color{javapurple}\bfseries,
stringstyle=\color{javared},
commentstyle=\color{javagreen},
morecomment=[s][\color{javadocblue}]{/**}{*/},
tabsize=3,
showspaces=false,
showstringspaces=false}

% Define the header format
\title{\vspace{-1.7cm}\textsc{\Title}}
\date{
  \Date\\
  \vspace{0.1cm}%
  \noindent\hfil\rule{\textwidth}{0.5pt}\hfil%
}
\author{\Me\ (\CRSID), \College}

\makeatletter
\let\mytitle\@title
\makeatother

%Redefine starred section to update leftmark
\let\CLASSsection\section
\RenewDocumentCommand{\section}{som}{%
  \IfBooleanTF{#1}
   {% there's a *
    \CLASSsection*{#3}\markboth{#3}{}%
   }
   {% no *
    \IfNoValueTF{#2}
     {% no opt arg
      \CLASSsection{#3}%
     }
     {% opt arg
      \CLASSsection[#2]{#3}%
     }%
   }%
}

\pagestyle{fancy}
\renewcommand{\sectionmark}[1]{\markright{\thesection\ #1}}

\fancyhf{}
\rhead{\textsc{\leftmark}}
\lhead{\mytitle}
\lfoot{\textsc{\Me}}
\rfoot{\textsc{Page \thepage\ of \pageref{LastPage}}}

%\renewcommand{\subsectionmark}[1]{}% Remove \subsection from header

\renewcommand{\headrulewidth}{0.5pt}
\renewcommand{\footrulewidth}{0.5pt}

\fancypagestyle{footeronly}
{
  \fancyhf{}
  \lfoot{\textsc{\Me}}
  \rfoot{\textsc{Page \thepage\ of \pageref{LastPage}}}
}

%Redefine title command to use the footeronly style
\let\oldtitle\maketitle
\renewcommand{\maketitle}[1]{\oldtitle\thispagestyle{footeronly}}

\usepackage[author={Craig Ferguson}]{pdfcomment}
\newcommand{\todo}{\pdfcomment[color=red, icon=insert]{Add content here...}}