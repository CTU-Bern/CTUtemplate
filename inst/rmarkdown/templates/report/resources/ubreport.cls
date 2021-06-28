%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Purpose: Class file, defines the layout of reports of the University of Bern
% Author: Lukas Buetikofer based on a template from Roger Schuerch
% Date: 12.10.2020
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%%%%%%%%%%%%%%%%%%%%%
% Identification
%%%%%%%%%%%%%%%%%%%%%

\NeedsTeXFormat{LaTeX2e}
\ProvidesClass{ubreport}[2016/05/06 v.0.1 Report Class for the University of Bern]
\typeout{Document class 'ubreport' for official reports in the layout}
\typeout{of the University of Bern}


%%%%%%%%%%%%%%%%%%%%%
% Initialization
%%%%%%%%%%%%%%%%%%%%

\RequirePackage{ifthen}
\newboolean{@color}
\setboolean{@color}{false}
\newdimen\leftfield
\newdimen\rightfield
\newdimen\openleftfield
\newdimen\openrightfield
\newdimen\leftfootfield
\newdimen\rightfootfield
\newdimen\fullwidthopen
\newdimen\fullwidthcont
\newdimen\boxseparation
\newcommand{\@reportlang}{0}
\newcommand{\@reportuser}{0}

% Declaration
\DeclareOption{english}{\renewcommand{\@reportlang}{0}}
\DeclareOption{german}{\renewcommand{\@reportlang}{1}}
\DeclareOption{french}{\renewcommand{\@reportlang}{2}}
\DeclareOption{personal}{\renewcommand{\@reportuser}{0}}
\DeclareOption{institute}{\renewcommand{\@reportuser}{1}}
\DeclareOption{color}{\setboolean{@color}{true}}
\DeclareOption*{\PassOptionsToClass{\CurrentOption}{report}}

% Execution
\ExecuteOptions{english}
\ProcessOptions

% Needed additional files
\LoadClass[a4paper]{report}
\RequirePackage{epsfig}
\RequirePackage{german}
%\RequirePackage[latin1]{inputenc}
%\RequirePackage[ansinew]{inputenc}
\RequirePackage{calc}
\RequirePackage{lastpage}
\RequirePackage{titlesec}


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Definition of user-specific items
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

\input{ubreport.clo}

\ifthenelse{\@reportuser=0}
	   	{\author{\sign}}
           	{}
\def\telephone{\telnum}

%language
\ifcase \@reportlang \reportenglish
\or \reportgerman
\or \RequirePackage{babel} \reportfrench
\fi

%selection of user
\ifcase \@reportuser \personalreport
\or \institutereport
\fi

%project
\project


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%general document layout
%%%%%%%%%%%%%%%%%%%%

% reset offset
\setlength{\hoffset}{-1in}
\setlength{\voffset}{-1in}

% set paper size according to Word template
% width
\setlength\textwidth{160mm}
\setlength\oddsidemargin{20mm}
\setlength\evensidemargin{30mm}
\setlength\marginparwidth{17.5mm}
% height
\setlength\textheight{215mm}
\setlength\topmargin{18mm}
\setlength\headheight{2\baselineskip}
\setlength\headsep{25mm}
\setlength\footskip{15mm}

% the custom stuff, logo, address, etc
\setlength{\leftfield}{70mm}       \setlength{\rightfield}{45mm}
\setlength{\openleftfield}{70mm}   \setlength{\openrightfield}{45mm}
\setlength{\leftfootfield}{40mm}   \setlength{\rightfootfield}{93mm}
\setlength{\fullwidthopen}{168mm}  \setlength{\fullwidthcont}{160.4mm}
\boxseparation=\fullwidthopen
   \advance\boxseparation by -\leftfield
   \advance\boxseparation by -\rightfield




%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% define header and footer
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

\ifthenelse{\equal{\version}{}}%
  {\def\headlist{\location, \today}}%
  {\def\headlist{\location, \today, \version}}


%header and footer for title page
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

\def\mainhead{%
	%header on title page
	\setlength{\unitlength}{1mm}
	\fontfamily{phv}\fontsize{6.7}{8.43}\selectfont
	\put(0,37.25){
		\parbox[t][][t]{\fullwidthopen}{
			\headlist \\
			\projectname: \reporttype \\
		}
	}
	%footer on title page
	\begin{picture}(-2,0)
		\put(0,-200){
		%\hspace{-3mm}
		\parbox[t][24mm][t]{\fullwidthopen}{%
			\raggedleft
			\parbox[t][24mm][t]{\rightfield}{%
			\fontfamily{phv}\fontsize{7.7}{9.43}\selectfont
			\sign\\
			\ifx\job\@empty \else \job\\ \fi
			\institute\\
			\mystreet\\
			\zip\ \location\\
			\ifx\telnum\@empty \else \telnum\\ \fi
			\ifx\email\@empty \else \email\\ \fi
			\ifx\web\@empty \else \web \fi
			}
		}
    }
	\end{picture}
	%logo
	\hspace{123mm}
	\parbox[b][44mm][t]{\fullwidthopen}{%
		\parbox[t][39mm][t]{\rightfield}{%
			\ifthenelse{\boolean{@color}}
			{\includegraphics[width=45mm]{ub_16pt-cmyk.pdf}}
			{\includegraphics[width=45mm]{ub_16pt-bl.pdf}}
		}
		\par
		\fontfamily{phv}\fontsize{8.0}{10.0}\selectfont
		\faculty\par\vspace{+6pt}
		\ifx\dept\@empty \else \dept\par\vspace{+5pt} \fi
		\raggedright\bfseries\institute\par
	}
}

\def\mainfoot{}


%header and footer for other pages
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

\def\conthead{%
	\fontfamily{phv}\fontsize{6.7}{8.43}\selectfont
	\put(0,75){												%100
		\parbox[t][][t]{\fullwidthopen}{
			\headlist \\
			\projectname: \reporttype  \\
		}
	}
	\parbox[b][32mm][t]{\fullwidthcont}{
		\raggedleft{%
			\ifthenelse{\boolean{@color}}
			{\includegraphics[width=27.4mm]{ub_16pt-cmyk.pdf}}
			{\includegraphics[width=27.4mm]{ub_16pt-bl.pdf}}
		}
	}
}

%right aligned page number
\def\contfoot{\fontfamily{phv}\fontsize{8}{10}\selectfont\raggedleft\hfill\pagename\
  \thepage/\pageref{LastPage}}


%header and footer for TOC
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

\def\tochead{
	\fontfamily{phv}\fontsize{6.7}{8.43}\selectfont
}

%right aligned page number
\def\tocfoot{\fontfamily{phv}\fontsize{8}{10}\selectfont\raggedleft\hfill\pagename\
  \thepage/\pageref{LastPage}}



%%%%%%%%%%%%%%
% Pagestyles
%%%%%%%%%%%%%%%%%%%%%%

%first page
\def\ps@firstpage{\def\@oddhead{\hbox to\textwidth{\mainhead\hss}}
 \def\@oddfoot{\hbox to\textwidth{\mainfoot\hss}}}

%normal page
\def\ps@headings{
	\def\@oddhead{\hbox to\textwidth{\conthead\hss}}
	\def\@oddfoot{\hbox to\textwidth{\contfoot\hss}}
	\let\@evenhead=\@oddhead
	\let\@evenfoot=\@oddfoot
	\setlength{\topmargin}{18mm}
	\setlength{\headheight}{8mm}
}

%TOC
\def\ps@tocheadings{
	\def\@oddhead{\hbox to\textwidth{\tochead\hss}}
	\def\@oddfoot{\hbox to\textwidth{\tocfoot\hss}}
	\let\@evenhead=\@oddhead
	\let\@evenfoot=\@oddfoot
	\setlength{\topmargin}{18mm}
	\setlength{\headheight}{8mm}
}



%%%%%%%%%%%%%%%%%%%%%%
%titlepage
%%%%%%%%%%%%%%%%%%%

\renewcommand*{\maketitle}{
	\begin{titlepage}
	\raggedright
	\thispagestyle{firstpage}
	\hbox to\textwidth{\hbox
	to\fullwidthopen{\vbox{\hspace*{124mm}
	\vspace{11mm}
	\vspace{-10pt}}}\hss}
	\vspace*{55mm}
	\fontfamily{phv}\fontsize{30}{44.55}\selectfont
	\projectname\par
	\vspace{9mm}
	\fontfamily{phv}\fontsize{18}{20.79}\selectfont
	\reporttype\par
	\vspace{9mm}
	\fontfamily{phv}\fontsize{14}{14.85}\selectfont
	\versiondate\par
	\end{titlepage}
	\clearpage
	\thispagestyle{empty}
}



%%%%%%%%%%%%%%%
%font
%%%%%%%%%%%%%%

\renewcommand{\familydefault}{phv}


%%%%%%%%%%%%%%%%%%%%%%%%%%
%chapter and sections
%%%%%%%%%%%%%%%%%%%%%%%


%depth of numbering and TOC:
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

\setcounter{secnumdepth}{4}
\setcounter{tocdepth}{4}


% title formats and spacing:
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

\titleformat{\chapter}{\thispagestyle{headings}\normalfont\LARGE\bfseries}{\thechapter}{1em}{}
\titlespacing*{\chapter}{0pt}{-2ex}{3ex plus .2ex}

\titleformat{\section}{\normalfont\large\bfseries}{\thesection}{1em}{}
\titlespacing*{\section}{0pt}{3.5ex plus 1ex minus .2ex}{3ex plus .2ex}

\titleformat{\subsection}{\normalfont\normalsize}{\thesubsection}{1em}{}
\titlespacing*{\subsection}{0pt}{3.5ex plus 1ex minus .2ex}{2.3ex plus .2ex}

\titleformat{\subsubsection}{\normalfont\normalsize}{\thesubsubsection}{1em}{}
\titlespacing*{\subsubsection}{0pt}{3.5ex plus 1ex minus .2ex}{2.3ex plus .2ex}

\providecommand{\tightlist}{%
  \setlength{\itemsep}{0pt}\setlength{\parskip}{0pt}}
