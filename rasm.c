/***

Rasm (roudoudou assembler) Z80 assembler v0.61

Check latest official release at: http://www.cpcwiki.eu/forum/programming/rasm-z80-assembler-in-beta/

You may send requests/bugs in the same topic

--------------------------------------------------

This software is using MIT "expat" license

« Copyright © BERGE Edouard (roudoudou)

Permission  is  hereby  granted,  free  of charge,
to any person obtaining a copy  of  this  software
and  associated  documentation/source   files   of
RASM, to deal in the Software without restriction,
including without limitation the  rights  to  use,
copy,   modify,   merge,   publish,    distribute,
sublicense,  and/or  sell  copies of the Software,
and  to  permit  persons  to  whom the Software is
furnished  to  do  so,  subject  to  the following
conditions:

The above copyright  notice  and  this  permission
notice   shall   be  included  in  all  copies  or
substantial portions of the Software.
The   Software   is   provided  "as is",   without
warranty   of   any   kind,  express  or  implied,
including  but  not  limited  to the warranties of
merchantability,   fitness   for   a    particular
purpose  and  noninfringement.  In  no event shall
the  authors  or  copyright  holders be liable for
any  claim, damages  or other  liability,  whether
in  an  action  of  contract, tort  or  otherwise,
arising from,  out of  or in connection  with  the
software  or  the  use  or  other  dealings in the
Software. »

--------------------------------------------------



Supported features:
-------------------

conditionnal loops and loops:

repeat <nn>
rend

repeat
until <condition>

while <condition>
wend

if/ifnot <condition> else elseif endif statement

switch/case/break/default/endswitch statement

a condition is defined by:
- equality with '=='
- greater '>'
- greater or equal '>='
- lower '<'
- lower or equal '<='


local labels with '@' in loops:

repeat 50
add hl,de
jr c,@jump
sbc hl,bc
@jump
rend

all complex undocumented instructions syntax:
res 0,(ix+0),a
bit 0,(ix+0),a
sll (ix+0),a
rl  (ix+0),a
rr  (ix+0),a
...

extended registers support:
ixl, xl or lx style

multiple instructions per line separated by 'two points':
example: nop:nop:nop:nop

almost 'free' style syntax:
label1 label2 label3 defb 5 + 12 ,'inutile'


supported operators:
* mul
/ div
+ add
- sub
following operators involve an integer conversion:
& and
| or
^ xor
§ mod
in maxam compatibility mode:
and
or
xor
mod

supported functions in expression:
sin   sinus
cos   cosinus
asin  arc sinus
acos  arc cosinus
atan  arc tangent
int   conversion from float to integer
floor conversion from float to first lower integer
abs   absolute value
ln    log neperien
log10 log base 10
exp   exponential
sqrt  square root

Assembly directives:
--------------------

Memory directives
protect <start>,<end> prevent memory region from writes, will trigger an error if so
align <nn>		next instruction/data adress will be aligned on <nn>
limit <nn>      maximum output adress (must be below or equal hardware limits)
org <nn>[,<nn>]	code adress, optionnaly followed by output adress
write direct <lower rom>,<upper rom>,<bank> for winape compatibility, bank parameter is set, this create a new memory space disregarding the RAM bank value

Define directives
<alias> equ <expression> create an alias of an expression
ds / defs <n1>,<n2>	define <n1> times the byte <n2>
db / defb		insert byte or string with quotes
dw / defw		insert word
defi			insert 32 bits integer
str             insert strings. The last character of each strings has his bit 7 forced to 1 (OR #80)
charset [p1,p2[,p3]]	same as Winape charset directive

Output file directives
amsdos			add an amsdos header to the outputed file
buildcpr		cartridge output mode
bank <nn>		in cartridge mode, select the bank where to write the code/data
namebank <nn>,<str>	in cartridge mode, set a name for the bank (for verbose purpose)

Input file & compression directives
read            include an ascii file
include			include an ascii file
incbin			include a binary file (extended parameters like Winape incbin directive: offset,size,offset_high)
inclz4			include a binary file and crunch with lz4
incl48			include a binary file and crunch with lz48
incl49			include a binary file and crunch with lz49
inczx7			include a binary file and crunch with zx7
incexo			include a binary file and crunch with exomizer
lzx7			open a zx7 crunched segment
lz4				open a lz48 crunched segment
lz48			open a lz48 crunched segment
lz49			open a lz49 crunched segment
lzexo			open an exomizer crunched segment
lzclose			close a crunched segment
save "file",adr,size	save memory region to a file

Loop and conditionnal directives
repeat / rend
repeat / until
while / wend
if/ifnot <condition> else elseif endif statement
switch/case/break/default/endswitch statement

Ignored directives:
list	        ignored
nolist	        ignored
brk             ignored
run <nn>        ignored*

Other directives
let             this useless keyword is ignored like in locomotive Basic
print			print var, text or expression
stop			stop assembling

command line options:
- symbol export in Winape/Pasmo format
- may also export local symbols, variables, alias EQU
- symbol import
- maxam compatibility (disable meaningless expression check)

limitations:
- unlimited sources/labels/variables/expressions size
- unlimited number of sources/labels/variables/expressions/loops
- non crunched blocks cannot exceed memory size (64K in classic mode) before compression. It's planed to fix this.
- Z80 instructions or directives cannot be used as label/variable
- Rasm is not case sensitive

-----------------------------------------------------

GCC compilation:
cc rasm_v061.c -O2 -lm -lrt
strip a.out
mv a.out rasm

Visual studio compilation:
cl.exe rasm_v061.c -O2

*/

#ifdef __WATCOMC__
#define OS_WIN 1
#endif

#ifdef _WIN32
#define OS_WIN 1
#endif

#ifdef _WIN64
#define OS_WIN 1
#endif

#ifndef RDD
	/* public lib */
	#include"minilib.h"
#else
	/* private dev lib wont be published */
	#include"../tools/library.h"
#endif

#ifndef NO_3RD_PARTIES
#define __FILENAME__ "3rd parties"
/* 3rd parties compression */
#include"zx7.h"
#include"lz4.h"
#include"exomizer.h"
#endif

#undef __FILENAME__
#define __FILENAME__ "rasm.c"

enum e_compute_evaluation_type {
E_COMPUTE_EVALUATION_GT,
E_COMPUTE_EVALUATION_GE,
E_COMPUTE_EVALUATION_LT,
E_COMPUTE_EVALUATION_LE,
E_COMPUTE_EVALUATION_NE,
E_COMPUTE_EVALUATION_EQ,
E_COMPUTE_EVALUATION_ASSIGN,
E_COMPUTE_EVALUATION_COMPUTE_ONLY
};

enum e_compute_operation_type {
E_COMPUTE_OPERATION_PUSH_DATASTC,
E_COMPUTE_OPERATION_ADD,
E_COMPUTE_OPERATION_SUB,
E_COMPUTE_OPERATION_DIV,
E_COMPUTE_OPERATION_MUL,
E_COMPUTE_OPERATION_AND,
E_COMPUTE_OPERATION_OR,
E_COMPUTE_OPERATION_MOD,
E_COMPUTE_OPERATION_XOR,
/* math functions */
E_COMPUTE_OPERATION_SIN,
E_COMPUTE_OPERATION_COS,
E_COMPUTE_OPERATION_INT,
E_COMPUTE_OPERATION_FLOOR,
E_COMPUTE_OPERATION_ABS,
E_COMPUTE_OPERATION_LN,
E_COMPUTE_OPERATION_LOG10,
E_COMPUTE_OPERATION_SQRT,
E_COMPUTE_OPERATION_ASIN,
E_COMPUTE_OPERATION_ACOS,
E_COMPUTE_OPERATION_ATAN,
E_COMPUTE_OPERATION_EXP,
E_COMPUTE_OPERATION_END
};

struct s_compute_compiled_operation {
enum e_compute_operation_type operation_type;
double data_static;
};

enum e_computing_error {
COMPUTING_OK=0,
COMPUTING_ERROR_CLOSING_PARENTHESIS,
COMPUTING_ERROR_INVALID_CHARACTER,
COMPUTING_ERROR_MINUS_WITHOUT_DATA,
COMPUTING_ERROR_END
};

enum e_expression {
	E_EXPRESSION_J8,
	E_EXPRESSION_0V8,
	E_EXPRESSION_V8,
	E_EXPRESSION_V16,
	E_EXPRESSION_0V16,
	E_EXPRESSION_0V32,
	E_EXPRESSION_IV8,
	E_EXPRESSION_3V8,
	E_EXPRESSION_IV16,
	E_EXPRESSION_RST,
	E_EXPRESSION_IM
};

struct s_expression {	
	char *reference; /* backup when used inside loop (or macro?) */
	int iw;          /* word index in the main wordlist */
	int o;           /* offset de depart 0, 1 ou 3 selon l'opcode */
	int ptr;         /* offset courant pour calculs relatifs */
	int wptr;        /* where to write the result  */
	enum e_expression zetype;
	int lz;
	int ibank;
	int iorgzone;
};

struct s_expr_dico {
	char *name;
	int crc;
	double v;
};

struct s_label {
	char *name; /* for local repeat */
	int iw;   /* index of the word of label name */
	int crc;  /* crc */
	int ptr;  /* "physical" adress */
	int lz;   /* is the label in a crunched section (or after)? */
	int iorgzone;
	int ibank; /* current CPR bank / always zero in classic mode */
};

struct s_alias {
	char *alias;
	char *translation;
	int crc,len;
};

struct s_crclabel_tree {
	struct s_crclabel_tree *radix[256];
	struct s_label *label;
	int nlabel,mlabel;
};
struct s_crcdico_tree {
	struct s_crcdico_tree *radix[256];
	struct s_expr_dico *dico;
	int ndico,mdico;
};
struct s_crcalias_tree {
	struct s_crcalias_tree *radix[256];
	struct s_alias *alias;
	int nalias,malias;
};


struct s_lz_section {
	int iw;
	int memstart,memend;
	int lzversion; /* 4 -> LZ4 / 48 -> LZ48 / 49 -> LZ49 */
	int iorgzone;
	int ibank;
	/* idx backup */
	int iexpr;
	int ilabel;
};

struct s_hexbin {
	unsigned char *data;
	int datalen;
};
struct s_orgzone {
	int ibank,protect;
	int memstart,memend;
	int ifile,iline;
};

struct s_save {
	int ibank;
	int ioffset;
	int isize;
	int iw;
};

struct s_repeat {
	int start;
	int cpt;
	int value;
	int maxim;
};

struct s_whilewend {
	int start;
	int cpt;
	int value;
	int maxim;
};

struct s_wordlist {
	char *w;
	int l,t,e; /* e=1 si egalite dans le mot */
	int ifile;
};

struct s_macro {
	char *mnemo;
	int crc;
	/* une macro concatene des chaines et des parametres */
	struct s_wordlist *wc;
	int nbword,maxword;
	/**/
	char **param;
	int nbparam;
};

struct s_macro_position {
	int start,end,value;
};

struct s_switchcase {
	int refval;
	int execute;
	int casematch;
};

/* preprocessing only */
struct s_macro_fast {
	char *mnemo;
	int crc;
};

#ifdef RASM_THREAD
struct s_rasm_thread {
	pthread_t thread;
	int lz;
	unsigned char *datain;
	int datalen;
	unsigned char *dataout;
	int lenout;
	int status;
};
#endif

struct s_assenv {
	/* current memory */
	int maxptr;
	/* CPR memory */
	unsigned char **mem;
	int iwnamebank[32];
	int nbbank,maxbank;
	int forcecpr,cprmode,activebank,amsdos,snapshot;
	/* parsing */
	struct s_wordlist *wl;
	int nbword;
	int idx,stage;
	char **filename;
	int ifile,maxfile;
	int nberr,flux,verbose;
	int fastmatch[256];
	unsigned char charset[256];
	/* ORG tracking */
	int codeadr,outputadr;
	int minadr,maxadr;
	struct s_orgzone *orgzone;
	int io,mo;
	/* expressions */
	struct s_expression *expression;
	int ie,me;
	int maxam;
	float rough;
	/* label */
	struct s_label *label;
	int il,ml;
	struct s_crclabel_tree labeltree; /* fast label access */
	char *module;
	/* repeat */
	struct s_repeat *repeat;
	int ir,mr;
	/* while/wend */
	struct s_whilewend *whilewend;
	int iw,mw;
	/* if/then/else */
	int *ifthen;
	int ii,mi;
	/* switch/case */
	struct s_switchcase *switchcase;
	int isw,msw;
	/* expression dictionnary */
	struct s_expr_dico *dico;
	int idic,mdic;
	struct s_crcdico_tree dicotree; /* fast dico access */
	/* crunch section flag */
	struct s_lz_section *lzsection;
	int ilz,mlz;
	int lz,curlz;
	/* macro */
	struct s_macro *macro;
	int imacro,mmacro;
	/* labels locaux */
	int repeatcounter,whilecounter,macrocounter;
	struct s_macro_position *macropos;
	int imacropos,mmacropos;
	/* alias */
	struct s_alias *alias;
	int ialias,malias;
	struct s_crcalias_tree aliastree; /* fast alias access */
	/* hexbin */
	struct s_rasm_thread **rasm_thread;
	int irt,mrt;
	struct s_hexbin *hexbin;
	int ih,mh;
	/* automates */
	char AutomateExpressionValidChar[256];
	char AutomateExpressionDecision[256];
	char AutomateValidLabelFirst[256];
	char AutomateValidLabel[256];
	/* output */
	char *outputfilename;
	int export_sym,export_local;
	int export_var,export_equ;
	char *symbol_name;
	char *binary_name;
	char *cartridge_name;
	struct s_save *save;
	int nbsave,maxsave;
};

struct s_asm_keyword {
	char *mnemo;
	int crc;
	void (*makemnemo)(struct s_assenv *ae);
};

struct s_math_keyword {
	char *mnemo;
	int crc;
	enum e_compute_operation_type operation;
};

struct s_expr_word {
	char *w;
	int aw;
	int op;
	int comma;
	int fct;
	double v;
};

struct s_lz_zone {
	unsigned char *mem;
	int lng;
};

struct s_symbol {
	int crc;
	char *zename;
	unsigned short int adr;
};

struct s_repeat_index {
	int ifile;
	int ol,oidx;
	int cl,cidx;
};

struct s_listing {
	char *listing;
	int ifile;
	int iline;
};


struct s_math_keyword math_keyword[]={
{"SIN",0,E_COMPUTE_OPERATION_SIN},
{"COS",0,E_COMPUTE_OPERATION_COS},
{"INT",0,E_COMPUTE_OPERATION_INT},
{"ABS",0,E_COMPUTE_OPERATION_ABS},
{"LN",0,E_COMPUTE_OPERATION_LN},
{"LOG10",0,E_COMPUTE_OPERATION_LOG10},
{"SQRT",0,E_COMPUTE_OPERATION_SQRT},
{"FLOOR",0,E_COMPUTE_OPERATION_FLOOR},
{"ASIN",0,E_COMPUTE_OPERATION_ASIN},
{"ACOS",0,E_COMPUTE_OPERATION_ACOS},
{"ATAN",0,E_COMPUTE_OPERATION_ATAN},
{"EXP",0,E_COMPUTE_OPERATION_EXP},
{"",0,-1}
};

#define CRC_SWITCH    0x01AEDE4A
#define CRC_CASE      0x0826B794
#define CRC_DEFAULT   0x9A0DAC7D
#define CRC_BREAK     0xCD364DDD
#define CRC_ENDSWITCH 0x18E9FB21

#define CRC_ELSEIF 0xE175E230
#define CRC_ELSE   0x3FF177A1
#define CRC_ENDIF  0xCD5265DE
#define CRC_IF     0x4BD52507
#define CRC_IFNOT  0x4CCAC9F8
#define CRC_WHILE  0xBC268FF1
#define CRC_UNTIL  0xCC12A604
#define CRC_MEND   0xFFFD899C
#define CRC_MACRO  0x64AA85EA

#define CRC_SIN 0xE1B71962
#define CRC_COS 0xE077C55D

#define CRC_0    0x7A98A6A8
#define CRC_1    0x7A98A6A9
#define CRC_2    0x7A98A6AA


#define CRC_NC   0x4BD52B09
#define CRC_Z    0x7A98A6D2
#define CRC_NZ   0x4BD52B20
#define CRC_P    0x7A98A6C8
#define CRC_PO   0x4BD53717
#define CRC_PE   0x4BD5370D
#define CRC_M    0x7A98A6C5

/* 8 bits registers */
#define CRC_F    0x7A98A6BE
#define CRC_I    0x7A98A6C1
#define CRC_R    0x7A98A6CA
#define CRC_A    0x7A98A6B9
#define CRC_B    0x7A98A6BA
#define CRC_C    0x7A98A6BB
#define CRC_D    0x7A98A6BC
#define CRC_E    0x7A98A6BD
#define CRC_H    0x7A98A6C0
#define CRC_L    0x7A98A6C4
/* dual naming */
#define CRC_XH   0x4BD50718
#define CRC_XL   0x4BD5071C
#define CRC_YH   0x4BD50519
#define CRC_YL   0x4BD5051D
#define CRC_HX   0x4BD52718
#define CRC_LX   0x4BD52F1C
#define CRC_HY   0x4BD52719
#define CRC_LY   0x4BD52F1D
#define CRC_IXL  0xE19F1765
#define CRC_IXH  0xE19F1761
#define CRC_IYL  0xE19F1166
#define CRC_IYH  0xE19F1162

/* 16 bits registers */
#define CRC_BC   0x4BD5D2FD
#define CRC_DE   0x4BD5DF01
#define CRC_HL   0x4BD5270C
#define CRC_IX   0x4BD52519
#define CRC_IY   0x4BD5251A
#define CRC_SP   0x4BD5311B
#define CRC_AF   0x4BD5D4FF
/* memory convention */
#define CRC_MHL  0xD0765F5D
#define CRC_MDE  0xD0467D52
#define CRC_MBC  0xD05E694E
#define CRC_MIX  0xD072B76A
#define CRC_MIY  0xD072B16B
#define CRC_MSP  0xD01A876C
#define CRC_MC   0xE018210C


/*
# base=16
% base=2
0-9 base=10
A-Z variable ou fonction (cos, sin, tan, sqr, pow, mod, and, xor, mod, ...)
+*-/&^§| operateur
*/

#define AutomateExpressionValidCharDefinition "#%0123456789.ABCDEFGHIJKLMNOPQRSTUVWXYZ_@${}"
#define AutomateValidLabelFirstDefinition ".ABCDEFGHIJKLMNOPQRSTUVWXYZ_@"
#define AutomateValidLabelDefinition "0123456789.ABCDEFGHIJKLMNOPQRSTUVWXYZ_@"

#ifndef NO_3RD_PARTIES
unsigned char *LZ4_crunch(unsigned char *data, int zelen, int *retlen){
	unsigned char *lzdest=NULL;
	lzdest=MemMalloc(65536);
	*retlen=LZ4_compress_HC((char*)data,(char*)lzdest,zelen,65536,9);
	return lzdest;
}
#endif
unsigned char *LZ48_encode_legacy(unsigned char *data, int length, int *retlength);
#define LZ48_crunch LZ48_encode_legacy
unsigned char *LZ49_encode_legacy(unsigned char *data, int length, int *retlength);
#define LZ49_crunch LZ49_encode_legacy


/***
	TxtReplace
	
	input:
	in_str:     string where replace will occur
	in_substr:  substring to look for
	out_substr: replace substring
	recurse:    loop until no in_substr is found
	
	note: in_str MUST BE previously mallocated if out_substr is bigger than in_substr
*/
#ifndef RDD
char *TxtReplace(char *in_str, char *in_substr, char *out_substr, int recurse)
{
	#undef FUNC
	#define FUNC "TxtReplace"
	
	char *str_look,*m1,*m2;
	char *out_str;
	int sl,l1,l2,dif,cpt;

	if (in_str==NULL)
		return NULL;
		
	sl=strlen(in_str);
	l1=strlen(in_substr);
	/* empty string, nothing to do except return empty string */
	if (!sl || !l1)
		return in_str;
		
	l2=strlen(out_substr);
	dif=l2-l1;
		
	/* replace string is small or equal in size, we dont realloc */
	if (dif<=0)
	{
		/* we loop while there is a replace to do */
		str_look=strstr(in_str,in_substr);
		while (str_look!=NULL)
		{
			/*logdebug(str_look);*/
			
			/* we copy the new string if his len is not null */
			if (l2)
				memcpy(str_look,out_substr,l2);
			/* only if len are different */
			if (l1!=l2)
			{
				/* we move the end of the string byte per byte
				   because memory locations overlap. This is
				   faster than memmove */
				m1=str_look+l1;
				m2=str_look+l2;
				while (*m1!=0)
				{
					*m2=*m1;
					m1++;m2++;
				}
				/* we must copy the EOL */
				*m2=*m1;
			}
			/* look for next replace */
			if (!recurse)
				str_look=strstr(str_look+l2,in_substr);
			else
				str_look=strstr(in_str,in_substr);
		}
		out_str=in_str;
	}
	else
	{
		/* we need to count each replace */
		cpt=0;
		str_look=strstr(in_str,in_substr);
		while (str_look!=NULL)
		{
			cpt++;
			str_look=strstr(str_look+l1,in_substr);
		}
		/* is there anything to do? */
		if (cpt)
		{
			/* we realloc to a size that will fit all replaces */
			out_str=MemRealloc(in_str,sl+1+dif*cpt);
			str_look=strstr(out_str,in_substr);
			while (str_look!=NULL && cpt)
			{
				/* as the replace string is bigger we
				   have to move memory first from the end */
				m1=out_str+sl;
				m2=m1+dif;
				sl+=dif;
				while (m1!=str_look+l1-dif)
				{
					*m2=*m1;
					m1--;m2--;
				}
				/* then we copy the replace string (can't be NULL in this case) */
				memcpy(str_look,out_substr,l2);
				
				/* look for next replace */
				if (!recurse)
					str_look=strstr(str_look+l2,in_substr);
				else
					str_look=strstr(in_str,in_substr);
					
				/* to prevent from naughty overlap */
				cpt--;
			}
			if (str_look!=NULL)
			{
				printf("INTERNAL ERROR - overlapping replace string (%s/%s), you can't use this one!\n",in_substr,out_substr);
				exit(ABORT_ERROR);
			}
		}
		else
			out_str=in_str;
	}
	return out_str;
}
#endif

#ifdef RASM_THREAD
/*
 threads used for crunching
*/
void _internal_ExecuteThreads(struct s_assenv *ae,struct s_rasm_thread *rasm_thread, void *(*fct)(void *))
{
	#undef FUNC
	#define FUNC "_internal_ExecuteThreads"

	pthread_attr_t attr;
	void *status;
	int rc;
	/* launch threads */
	pthread_attr_init(&attr);
	pthread_attr_setdetachstate(&attr,PTHREAD_CREATE_JOINABLE);
	pthread_attr_setstacksize(&attr,65536);

	if ((rc=pthread_create(&image_threads[i].thread,&attr,fct,(void *)rasm_thread))) {
		rasm_printf(ae,"FATAL ERROR - Cannot create thread!\n");
		exit(INTERNAL_ERROR);
	}
}
void _internal_WaitForThreads(struct s_assenv *ae,struct s_rasm_thread *rasm_thread)
{
	#undef FUNC
	#define FUNC "_internal_WaitForThreads"
	int rc;
	
	if ((rc=pthread_join(rasm_thread->thread,&status))) {
		rasm_printf(ae,"FATAL ERROR - Cannot wait for thread\n");
		exit(INTERNAL_ERROR);
	}
}
void PushCrunchedFile(struct s_assenv *ae, unsigned char *datain, int datalen, int lz)
{
	#undef FUNC
	#define FUNC "PushCrunchedFile"
	
	struct s_rasm_thread *rasm_thread;
	
	rasm_thread=MemMalloc(sizeof(struct s_rasm_thread));
	memset(rasm_thread,0,sizeof(struct s_rasm_thread));
	rasm_thread->datain=datain;
	rasm_thread->datalen=datalen;
	rasm_thread->lz=lz;
	_internal_ExecuteThreads(ae,rasm_thread, void *(*fct)(void *));
	ObjectArrayAddDynamicValueConcat((void**)&ae->rasm_thread,&ae->irt,&ae->mrt,&rasm_thread,sizeof(struct s_rasm_thread *));
}
void PopAllCrunchedFiles(struct s_assenv *ae)
{
	#undef FUNC
	#define FUNC "PopAllCrunchedFiles"
	
	int i;
	for (i=0;i<ae->irt;i++) {
		_internal_WaitForThreads(ae,ae->rasm_thread[i]);
	}
}
#endif


void rasm_printf(struct s_assenv *ae, ...) {
	#undef FUNC
	#define FUNC "rasm_printf"
	
	char *format;
	va_list argptr;

	if (!ae->flux) {
		va_start(argptr,ae);
		format=va_arg(argptr,char *);
		vfprintf(stdout,format,argptr);	
		va_end(argptr);
	}
}


char *GetPath(char *filename) {
	#undef FUNC
	#define FUNC "GetPath"

	static char curpath[PATH_MAX];
	int zelen,idx;

	zelen=strlen(filename);

#ifdef OS_WIN
	#define CURRENT_DIR ".\\"

	TxtReplace(filename,"/","\\",1);
	idx=zelen-1;
	while (idx>=0 && filename[idx]!='\\') idx--;
	if (idx<0) {
		/* pas de chemin */
		strcpy(curpath,".\\");
	} else {
		/* chemin trouve */
		strcpy(curpath,filename);
		curpath[idx+1]=0;
	}
#else
	#define CURRENT_DIR "./"
	idx=zelen-1;
	while (idx>=0 && filename[idx]!='/') idx--;
	if (idx<0) {
		/* pas de chemin */
		strcpy(curpath,"./");
	} else {
		/* chemin trouve */
		strcpy(curpath,filename);
		curpath[idx+1]=0;
	}
#endif

	return curpath;
}
char *MergePath(struct s_assenv *ae,char *dadfilename, char *filename) {
	#undef FUNC
	#define FUNC "MergePath"

	static char curpath[PATH_MAX];
	int zelen;

	zelen=strlen(filename);
#ifdef OS_WIN
	TxtReplace(filename,"/","\\",1);

	if (filename[0] && filename[1]==':' && filename[2]=='\\') {
		/* chemin absolu */
		strcpy(curpath,filename);
	} else if (filename[0] && filename[1]==':') {
		rasm_printf(ae,"unsupported path style [%s]\n",filename);
		exit(-111);
	} else {
		if (filename[0]=='.' && filename[1]=='\\') {
			strcpy(curpath,GetPath(dadfilename));
			strcat(curpath,filename+2);
		} else {
			strcpy(curpath,GetPath(dadfilename));
			strcat(curpath,filename);
		}
	}
#else
	if (filename[0]=='/') {
		/* chemin absolu */
		strcpy(curpath,filename);
	} else if (filename[0]=='.' && filename[1]=='/') {
		strcpy(curpath,GetPath(dadfilename));
		strcat(curpath,filename+2);
	} else {
		strcpy(curpath,GetPath(dadfilename));
		strcat(curpath,filename);
	}
#endif

	return curpath;
}


void InitAutomate(char *autotab, const char *def)
{
	#undef FUNC
	#define FUNC "InitAutomate"

	int i;

	memset(autotab,0,256);
	for (i=0;def[i];i++) autotab[(int)def[i]]=1;
}
void StateMachineResizeBuffer(char **ABuf, int idx, int *ASize) {
	#undef FUNC
	#define FUNC "StateMachineResizeBuffer"

	if (idx>=*ASize) {
		if (*ASize<16384) {
			*ASize=(*ASize)*2;
		} else {
			*ASize=(*ASize)+16384;
		}
		*ABuf=MemRealloc(*ABuf,(*ASize)+2);
	}
}

int GetCRC(char *label)
{
	#undef FUNC
	#define FUNC "GetCRC"
	int crc=0x12345678;
	int i=0;

	while (label[i]!=0) {
		crc=(crc<<9)^(crc+label[i++]);
	}
	return crc;
}

int StringIsMem(char *w)
{
	#undef FUNC
	#define FUNC "StringIsMem"

	int p=1,idx=1;

	if (w[0]=='(') {
		while (w[idx]) {
			switch (w[idx]) {
				case '\\':if (w[idx+1]) idx++;
					break;
				case '\'':if (w[idx+1] && w[idx+1]!='\\') idx++;
					break;
				case '(':p++;break;
				case ')':p--;
					if (!p && w[idx+1]) return 0;
					break;
				default:break;
			}
			idx++;
		}
		if (w[idx-1]!=')') return 0;
	} else {
		return 0;
	}
	return 1;

}
int StringIsQuote(char *w)
{
	#undef FUNC
	#define FUNC "StringIsQuote"

	int i,tquote,lens;

	if (w[0]=='\'' || w[0]=='"') {
		tquote=w[0];
		lens=strlen(w);
		
		/* est-ce bien une chaine et uniquement une chaine? */
		i=1;
		while (w[i] && w[i]!=tquote) {
			if (w[i]=='\\') i++;
			i++;
		}
		if (i==lens-1) {
			return tquote;
		}
	}
	return 0;
}

void MaxError(struct s_assenv *ae)
{
	#undef FUNC
	#define FUNC "MaxError"
	
	ae->nberr++;
	if (ae->nberr==50) {
		rasm_printf(ae,"Too many errors!\n");
		exit(1);
	}
}

char *GetExpFile(struct s_assenv *ae,int didx){
	#undef FUNC
	#define FUNC "GetExpFile"
	
	if (!didx) {
		return ae->filename[ae->wl[ae->idx].ifile];
	} else {
		if (ae->expression && didx<ae->ie) {
			return ae->filename[ae->wl[ae->expression[didx].iw].ifile];
		} else {
			return ae->filename[ae->wl[ae->idx].ifile];
		}
	}
}

int GetExpLine(struct s_assenv *ae,int didx){
	#undef FUNC
	#define FUNC "GetExpLine"
	
	if (!didx) {
		return ae->wl[ae->idx].l;
	} else {
		return ae->wl[ae->expression[didx].iw].l;
	}
}

char *GetCurrentFile(struct s_assenv *ae)
{
	return GetExpFile(ae,0);
}

void ___output(struct s_assenv *ae,unsigned char v)
{
	#undef FUNC
	#define FUNC "___output"
	
	if (ae->outputadr<=ae->maxptr) {
		ae->mem[ae->activebank][ae->outputadr++]=v;
		ae->codeadr++;
	} else {
		rasm_printf(ae,"[%s] Error line %d - output exceed limit %d\n",GetCurrentFile(ae),ae->wl[ae->idx].l,ae->maxptr);
		exit(3);
	}
}

void ___output_set_limit(struct s_assenv *ae,int zelimit)
{
	#undef FUNC
	#define FUNC "___output_set_limit"

	int limit=65536;
	
	if (zelimit<=limit) {
		/* apply limit */
		limit=zelimit;
	} else {
		rasm_printf(ae,"[%s] Error line %d - limit exceed hardware limitation!\n",GetCurrentFile(ae),ae->wl[ae->idx].l);
		MaxError(ae);
	}
	if (ae->outputadr>=0 && ae->outputadr>limit) {
		rasm_printf(ae,"[%s] Error line %d - limit too high for current output!\n",GetCurrentFile(ae),ae->wl[ae->idx].l);
		MaxError(ae);
	}
	ae->maxptr=limit;
}

void ExpressionFastTranslate(struct s_assenv *ae, char **ptr_expr, int fullreplace);

int cmpmacros(const void * a, const void * b)
{
	struct s_macro *sa,*sb;
	sa=(struct s_macro *)a;
	sb=(struct s_macro *)b;
	if (sa->crc<sb->crc) return -1; else return 1;
}
int SearchAlias(struct s_assenv *ae, int crc, char *zemot)
{
    int dw,dm,du,i;

	/* inutile de tourner autour du pot pour un si petit nombre */
	if (ae->ialias<5) {
			for (i=0;i<ae->ialias;i++) {
					if (ae->alias[i].crc==crc && strcmp(ae->alias[i].alias,zemot)==0) {
							return i;
					}
			}
			return -1;
	}
	
	dw=0;
	du=ae->ialias-1;
	while (dw<=du) {
		dm=(dw+du)/2;
		if (ae->alias[dm].crc==crc) {
			/* chercher le premier de la liste */
			while (dm>0 && ae->alias[dm-1].crc==crc) dm--;
			/* controle sur le texte entier */
			while (ae->alias[dm].crc==crc && strcmp(ae->alias[dm].alias,zemot)) dm++;
			if (ae->alias[dm].crc==crc && strcmp(ae->alias[dm].alias,zemot)==0) return dm; else return -1;
		} else if (ae->alias[dm].crc>crc) {
			du=dm-1;
		} else if (ae->alias[dm].crc<crc) {
			dw=dm+1;
		}
	}
	return -1;
}
int SearchMacro(struct s_assenv *ae, int crc, char *zemot)
{
	int dw,dm,du,i;

	/* inutile de tourner autour du pot pour un si petit nombre */
	if (ae->imacro<5) {
			for (i=0;i<ae->imacro;i++) {
					if (ae->macro[i].crc==crc && strcmp(ae->macro[i].mnemo,zemot)==0) {
							return i;
					}
			}
			return -1;
	}
	
	dw=0;
	du=ae->imacro-1;
	while (dw<=du) {
		dm=(dw+du)/2;
		if (ae->macro[dm].crc==crc) {
			/* chercher le premier de la liste */
			while (dm>0 && ae->macro[dm-1].crc==crc) dm--;
			/* controle sur le texte entier */
			while (ae->macro[dm].crc==crc && strcmp(ae->macro[dm].mnemo,zemot)) dm++;
			if (ae->macro[dm].crc==crc && strcmp(ae->macro[dm].mnemo,zemot)==0) return dm; else return -1;
		} else if (ae->macro[dm].crc>crc) {
			du=dm-1;
		} else if (ae->macro[dm].crc<crc) {
			dw=dm+1;
		}
	}
	return -1;
}

void CheckAndSortAliases(struct s_assenv *ae)
{
	#undef FUNC
	#define FUNC "CheckAndSortAliases"

	struct s_alias tmpalias;
	int i,dw,dm,du,crc;
	for (i=0;i<ae->ialias-1;i++) {
		/* est-ce qu'on retrouve des anciens alias dans le nouveau ? */
		if (strstr(ae->alias[ae->ialias-1].translation,ae->alias[i].alias)) {
			/* peut-être, on s'en assure  */
			ExpressionFastTranslate(ae,&ae->alias[ae->ialias-1].translation,2);
			ae->alias[ae->ialias-1].len=strlen(ae->alias[ae->ialias-1].translation);
			break;
		}
	}
	
	/* cas particuliers pour insertion en début ou fin de liste */
	if (ae->ialias-1) {
		if (ae->alias[ae->ialias-1].crc>ae->alias[ae->ialias-2].crc) {
			/* pas de tri il est déjà au bon endroit */
		} else if (ae->alias[ae->ialias-1].crc<ae->alias[0].crc) {
			/* insertion tout en bas de liste */
			tmpalias=ae->alias[ae->ialias-1];
			MemMove(&ae->alias[1],&ae->alias[0],sizeof(struct s_alias)*(ae->ialias-1));
			ae->alias[0]=tmpalias;
		} else {
			/* on cherche ou inserer */
			crc=ae->alias[ae->ialias-1].crc;
			dw=0;
			du=ae->ialias-1;
			while (dw<=du) {
				dm=(dw+du)/2;
				if (ae->alias[dm].crc==crc) {
					break;
				} else if (ae->alias[dm].crc>crc) {
					du=dm-1;
				} else if (ae->alias[dm].crc<crc) {
					dw=dm+1;
				}
			}
			/* ajustement */
			if (ae->alias[dm].crc<crc) dm++;
			/* insertion */
			tmpalias=ae->alias[ae->ialias-1];
			MemMove(&ae->alias[dm+1],&ae->alias[dm],sizeof(struct s_alias)*(ae->ialias-1-dm));
			ae->alias[dm]=tmpalias;
		}
	} else {
		/* pas de tri pour un seul élément */
	}
}

void InsertDicoToTree(struct s_assenv *ae, struct s_expr_dico *dico)
{
	#undef FUNC
	#define FUNC "InsertDicoToTree"

	struct s_crcdico_tree *curdicotree;
	int radix,dek=32;

	curdicotree=&ae->dicotree;
	while (dek) {
		dek=dek-8;
		radix=(dico->crc>>dek)&0xFF;
		if (curdicotree->radix[radix]) {
			curdicotree=curdicotree->radix[radix];
		} else {
			curdicotree->radix[radix]=MemMalloc(sizeof(struct s_crcdico_tree));
			curdicotree=curdicotree->radix[radix];
			memset(curdicotree,0,sizeof(struct s_crcdico_tree));
		}
	}
	ObjectArrayAddDynamicValueConcat((void**)&curdicotree->dico,&curdicotree->ndico,&curdicotree->mdico,dico,sizeof(struct s_expr_dico));
}

void ExportDicoTreeRecurse(struct s_crcdico_tree *lt, char *zefile, char *zeformat)
{
	#undef FUNC
	#define FUNC "ExportDicoTreeRecurse"

	char symbol_line[1024];
	int i;

	for (i=0;i<256;i++) {
		if (lt->radix[i]) {
			ExportDicoTreeRecurse(lt->radix[i],zefile,zeformat);
		}
	}
	if (lt->mdico) {
		for (i=0;i<lt->ndico;i++) {(lt->dico[i].name);
			sprintf(symbol_line,zeformat,lt->dico[i].name,(int)floor(lt->dico[i].v+0.5));
			FileWriteLine(zefile,symbol_line);
		}
	}
}
void ExportDicoTree(struct s_assenv *ae, char *zefile, char *zeformat)
{
	#undef FUNC
	#define FUNC "ExportDicoTree"

	int i;

	for (i=0;i<256;i++) {
		if (ae->dicotree.radix[i]) {
			ExportDicoTreeRecurse(ae->dicotree.radix[i],zefile,zeformat);
		}
	}
}
void FreeDicoTreeRecurse(struct s_crcdico_tree *lt)
{
	#undef FUNC
	#define FUNC "FreeDicoTreeRecurse"

	int i;

	for (i=0;i<256;i++) {
		if (lt->radix[i]) {
			FreeDicoTreeRecurse(lt->radix[i]);
		}
	}
	if (lt->mdico) {
		for (i=0;i<lt->ndico;i++) MemFree(lt->dico[i].name);
		MemFree(lt->dico);
	}
	MemFree(lt);
}
void FreeDicoTree(struct s_assenv *ae)
{
	#undef FUNC
	#define FUNC "FreeDicoTree"

	int i;

	for (i=0;i<256;i++) {
		if (ae->dicotree.radix[i]) {
			FreeDicoTreeRecurse(ae->dicotree.radix[i]);
		}
	}
	if (ae->dicotree.mdico) MemFree(ae->dicotree.dico);
}
struct s_expr_dico *SearchDico(struct s_assenv *ae, char *dico, int crc)
{
	#undef FUNC
	#define FUNC "SearchDico"

	struct s_crcdico_tree *curdicotree;
	struct s_expr_dico *retdico=NULL;
	int i,radix,dek=32;

	curdicotree=&ae->dicotree;

	while (dek) {
		dek=dek-8;
		radix=(crc>>dek)&0xFF;
		if (curdicotree->radix[radix]) {
			curdicotree=curdicotree->radix[radix];
		} else {
			/* radix not found, dico is not in index */
			return NULL;
		}
	}
	for (i=0;i<curdicotree->ndico;i++) {
		if (strcmp(curdicotree->dico[i].name,dico)==0) {
			return &curdicotree->dico[i];
		}
	}
	return NULL;
}

void FreeLabelTreeRecurse(struct s_crclabel_tree *lt)
{
	#undef FUNC
	#define FUNC "FreeLabelTreeRecurse"

	int i;

	for (i=0;i<256;i++) {
		if (lt->radix[i]) {
			FreeLabelTreeRecurse(lt->radix[i]);
		}
	}
	if (lt->mlabel) MemFree(lt->label);
	MemFree(lt);
}
void FreeLabelTree(struct s_assenv *ae)
{
	#undef FUNC
	#define FUNC "FreeLabelTree"

	int i;

	for (i=0;i<256;i++) {
		if (ae->labeltree.radix[i]) {
			FreeLabelTreeRecurse(ae->labeltree.radix[i]);
		}
	}
	if (ae->labeltree.mlabel) MemFree(ae->labeltree.label);
}

struct s_label *SearchLabel(struct s_assenv *ae, char *label, int crc)
{
	#undef FUNC
	#define FUNC "SearchLabel"

	struct s_crclabel_tree *curlabeltree;
	struct s_label *retlabel=NULL;
	int i,radix,dek=32;

	curlabeltree=&ae->labeltree;

	while (dek) {
		dek=dek-8;
		radix=(crc>>dek)&0xFF;
		if (curlabeltree->radix[radix]) {
			curlabeltree=curlabeltree->radix[radix];
		} else {
			/* radix not found, label is not in index */
			return NULL;
		}
	}
	for (i=0;i<curlabeltree->nlabel;i++) {
		if (!curlabeltree->label[i].name && strcmp(ae->wl[curlabeltree->label[i].iw].w,label)==0) {
			return &curlabeltree->label[i];
		} else if (curlabeltree->label[i].name && strcmp(curlabeltree->label[i].name,label)==0) {
			return &curlabeltree->label[i];
		}
	}
	return NULL;
}

char *MakeLocalLabel(struct s_assenv *ae,char *varbuffer, int *retdek)
{
	
	#undef FUNC
	#define FUNC "MakeLocalLabel"
	
	char *locallabel;
	char hexdigit[32];
	int lenbuf=0,dek,i,im;
//int debug;
//if (strncmp(varbuffer,"@FAST",5)==0) debug=1; else debug=0;
//if (debug) printf("[%s]\n",varbuffer);	
	lenbuf=strlen(varbuffer);
	if (!retdek) {
		locallabel=MemMalloc(lenbuf+(ae->ir+ae->iw+3)*8+8);
		strcpy(locallabel,varbuffer);
	} else {
		locallabel=MemMalloc((ae->ir+ae->iw+3)*8+4);
		locallabel[0]=0;
	}	
//if (debug) printf("[%s]\n",locallabel);	

	dek=0;
	dek+=strappend(locallabel,"R");
	for (i=0;i<ae->ir;i++) {
		sprintf(hexdigit,"%04X",ae->repeat[i].cpt&0xFFFF);
		dek+=strappend(locallabel,hexdigit);
	}
	if (ae->ir) {
		sprintf(hexdigit,"%04X",ae->repeat[ae->ir-1].value&0xFFFF);
		dek+=strappend(locallabel+dek,hexdigit);
	}
	
	dek+=strappend(locallabel,"W");
	for (i=0;i<ae->iw;i++) {
		sprintf(hexdigit,"%04X",ae->whilewend[i].cpt&0xFFFF);
		dek+=strappend(locallabel+dek,hexdigit);
	}
	if (ae->iw) {
		sprintf(hexdigit,"%04X",ae->whilewend[ae->iw-1].value&0xFFFF);
		dek+=strappend(locallabel+dek,hexdigit);
	}
	/* where are we? */
	if (ae->imacropos) {
		for (im=ae->imacropos-1;im>=0;im--) {
			if (ae->idx>=ae->macropos[im].start && ae->idx<ae->macropos[im].end) break;
		}
		if (im>=0) {
			/* si on n'est pas dans une macro, on n'indique rien */
			sprintf(hexdigit,"M%04X",ae->macropos[im].value&0xFFFF);
			dek+=strappend(locallabel+dek,hexdigit);
		}
	}
	if (retdek)	{
		*retdek=dek;
	}
	return locallabel;
}

double ComputeExpressionCore(struct s_assenv *ae,char *original_zeexpression,int ptr, int didx)
{
	#undef FUNC
	#define FUNC "ComputeExpressionCore"

	static struct s_compute_compiled_operation *operation=NULL;
	static int maxoperation=0;
	int nboperation=0;
	struct s_compute_compiled_operation compiled_operation={0};
	enum e_computing_error computing_error=COMPUTING_OK;
	static char *varbuffer=NULL;
	static int ivar=0,maxivar=1;
	int curoperator,operator1,operator2,lookforward=0;
	int trigger=0,lowflag=0;
	int idx=0,crc,icheck;
	char c;
	/* backup alias replace */
	char *zeexpression,*expr;
	int original=1;
	int ialias,startvar;
	int newlen,lenw;
	/* dictionnary */
	struct s_expr_dico *curdic;
	struct s_label *curlabel;
	int minusptr,imkey,bank;
	double curval;
	/* dynamic isolation */
	static int max_isolation_depth=0;
	static int *lookf_backup=NULL;
	static int *oper2_backup=NULL;
	static int *oper1_backup=NULL;
	static int *lowfl_backup=NULL;
	static int *trigg_backup=NULL;
	static int *funct_backup=NULL;
	int isolation_end_flag=0;
	int isolation=0;
	/* negative value */
	int allow_minus_as_sign=0;
	/* execution */
	static double *accu=NULL;
	static int maccu=0;
	int i,paccu=0;

	/* memory cleanup */
	if (!ae) {
		if (lookf_backup) { 
			MemFree(lookf_backup);lookf_backup=NULL;
			MemFree(oper1_backup);oper1_backup=NULL;
			MemFree(oper2_backup);oper2_backup=NULL;
			MemFree(lowfl_backup);lowfl_backup=NULL;
			MemFree(trigg_backup);trigg_backup=NULL;
			MemFree(funct_backup);funct_backup=NULL;
		}
		if (operation) MemFree(operation);operation=NULL;
		if (varbuffer) MemFree(varbuffer);varbuffer=NULL;
		if (maccu) MemFree(accu);accu=NULL;
		max_isolation_depth=0;
		maxoperation=0;
		maxivar=1;
		maccu=0;
		ivar=0;
		return 0.0;
	}
	/* be sure to have at least some bytes allocated */
	StateMachineResizeBuffer(&varbuffer,128,&maxivar);

	zeexpression=original_zeexpression;
	if (!zeexpression[0]) {
		return 0;
	}
	
	if (zeexpression[0]=='-') {
		compiled_operation.operation_type=E_COMPUTE_OPERATION_PUSH_DATASTC;
		compiled_operation.data_static=0.0;
		ObjectArrayAddDynamicValueConcat((void **)&operation,&nboperation,&maxoperation,&compiled_operation,sizeof(compiled_operation));
	}

	/* is there ascii char? */
	while ((c=zeexpression[idx])!=0) {
		if (c=='\'' || c=='"') {
			/* echappement */
			if (zeexpression[idx+1]=='\\') {
				if (zeexpression[idx+2] && zeexpression[idx+3]==c) {
					snprintf(zeexpression+idx,4,"#%03X",zeexpression[idx+2]);
					idx+=3;
				} else {
					rasm_printf(ae,"[%s] Error line %d - Only single escaped char may be quoted [%s]\n",GetExpFile(ae,didx),GetExpLine(ae,didx),zeexpression);
					MaxError(ae);
					zeexpression[0]=0;
					return 0;
				}
			} else if (zeexpression[idx+1] && zeexpression[idx+2]==c) {
					snprintf(zeexpression+idx,3,"#%02X",zeexpression[idx+2]);
					idx+=2;
			} else {
				rasm_printf(ae,"[%s] Error line %d - Only single char may be quoted [%s]\n",GetExpFile(ae,didx),GetExpLine(ae,didx),zeexpression);
				MaxError(ae);
				zeexpression[0]=0;
				return 0;
			}
		}
		
		idx++;
	}
	
	idx=operator1=operator2=0;
	while ((c=zeexpression[idx])!=0) {
		if (isolation_end_flag) {
			isolation_end_flag=0;
			lookforward=lookf_backup[isolation];
			operator1=oper1_backup[isolation];
			operator2=oper2_backup[isolation];
			lowflag=lowfl_backup[isolation];
			trigger=trigg_backup[isolation];
			
			if (funct_backup[isolation]) {
				compiled_operation.operation_type=funct_backup[isolation];
				ObjectArrayAddDynamicValueConcat((void **)&operation,&nboperation,&maxoperation,&compiled_operation,sizeof(compiled_operation));
			}
			if (trigger) {
				if (lookforward==2) {
					curoperator=operator2;
					lookforward=1;
				} else {
					curoperator=operator1;
					lookforward=0;
				}
				switch (curoperator) {
					case '+':compiled_operation.operation_type=E_COMPUTE_OPERATION_ADD;break;
					case '-':compiled_operation.operation_type=E_COMPUTE_OPERATION_SUB;break;
					case '*':compiled_operation.operation_type=E_COMPUTE_OPERATION_MUL;break;
					case '/':compiled_operation.operation_type=E_COMPUTE_OPERATION_DIV;break;
					case '&':compiled_operation.operation_type=E_COMPUTE_OPERATION_AND;break;
					case '§':compiled_operation.operation_type=E_COMPUTE_OPERATION_MOD;break;
					case '|':compiled_operation.operation_type=E_COMPUTE_OPERATION_OR;break;
					case '^':compiled_operation.operation_type=E_COMPUTE_OPERATION_XOR;break;
				}
				ObjectArrayAddDynamicValueConcat((void **)&operation,&nboperation,&maxoperation,&compiled_operation,sizeof(compiled_operation));
				trigger=0;
			}

		} else
		switch (c) {
			/* parenthesis */
			case '(':
			case '[':
				if (isolation>=max_isolation_depth) {
					max_isolation_depth+=16;
					lookf_backup=MemRealloc(lookf_backup,sizeof(int)*max_isolation_depth);
					oper1_backup=MemRealloc(oper1_backup,sizeof(int)*max_isolation_depth);
					oper2_backup=MemRealloc(oper2_backup,sizeof(int)*max_isolation_depth);
					lowfl_backup=MemRealloc(lowfl_backup,sizeof(int)*max_isolation_depth);
					trigg_backup=MemRealloc(trigg_backup,sizeof(int)*max_isolation_depth);
					funct_backup=MemRealloc(funct_backup,sizeof(int)*max_isolation_depth);
				}
				lookf_backup[isolation]=lookforward;lookforward=0;
				oper1_backup[isolation]=operator1;
				oper2_backup[isolation]=operator2;
				lowfl_backup[isolation]=lowflag;lowflag=0;
				trigg_backup[isolation]=trigger;trigger=0;
				funct_backup[isolation]=0;
				isolation++;
				allow_minus_as_sign=1;
				break;
			case ')':
			case ']':
				isolation--;
				if (isolation<0) computing_error=COMPUTING_ERROR_CLOSING_PARENTHESIS;
				trigger=1;
				isolation_end_flag=1;
				allow_minus_as_sign=0;
				break;
			/* operator detection */
			case '*':
			case '&':
			case '^':
			case '/':
				trigger=1;
				if (lookforward==0) {
					operator1=c;
					lookforward=1;
				} else if (lookforward==1) {
					operator2=c;
					lookforward=2;
				} else {
					rasm_printf(ae,"ComputeExpressionCore - INTERNAL ERROR\n");exit(-59);
				}
				allow_minus_as_sign=1;
				break;
			/* low priority operator */
			case '§':
			case '|':
			case '+':
			case '-':
				if (allow_minus_as_sign && c=='-') {
					/* previous char was an opening parenthesis or an operator */
					ivar=0;
					varbuffer[ivar++]='-';
					StateMachineResizeBuffer(&varbuffer,ivar,&maxivar);
					c=zeexpression[++idx];
					while (ae->AutomateExpressionValidChar[(int)c]) {
						varbuffer[ivar++]=c;
						StateMachineResizeBuffer(&varbuffer,ivar,&maxivar);
						idx++;
						c=zeexpression[idx];
					}
					varbuffer[ivar]=0;
					if (ivar<2) {
						computing_error=COMPUTING_ERROR_MINUS_WITHOUT_DATA;
					}
					break;
				}
				
				if (ae->maxam) {
					/* MAXAM shit */
					trigger=1;
					if (lookforward==0) {
						operator1=c;
						lookforward=1;
					} else if (lookforward==1) {
						operator2=c;
						lookforward=2;
					} else {
						rasm_printf(ae,"ComputeExpressionCore - Maxam shit INTERNAL ERROR\n");exit(-59);
					}
				}
				else {
					/* RASM classic */
					if (lookforward==0) {
						operator1=c;
						lookforward=1;
					} else if (lookforward==1) {
						operator2=c;
						lookforward=2;
						trigger=1;
						lowflag=1;
					} else {
						rasm_printf(ae,"Internal error on expression [%s]\n",zeexpression);
						exit(-44);
					}
				}
				allow_minus_as_sign=1;
				break;
			default:
				allow_minus_as_sign=0;
				/* semantic analysis */
				startvar=idx;
				ivar=0;
				while (ae->AutomateExpressionValidChar[((int)c)&0xFF]) {
					varbuffer[ivar++]=c;
					StateMachineResizeBuffer(&varbuffer,ivar,&maxivar);
					idx++;
					c=zeexpression[idx];
				}
				varbuffer[ivar]=0;
				if (!ivar) {
					computing_error=COMPUTING_ERROR_INVALID_CHARACTER;
					break; /* debile */
				}
		}
		if (c && !ivar && !isolation_end_flag) idx++;
	
		/* must compute all opened operators! */
		if (isolation_end_flag) {
		
			if (lookforward) {
				if (lookforward==2) {
					curoperator=operator2;
					lookforward=1;
				} else if (lookforward==1) {
					curoperator=operator1;
					lookforward=0;
				}
				switch (curoperator) {
					case '+':compiled_operation.operation_type=E_COMPUTE_OPERATION_ADD;break;
					case '-':compiled_operation.operation_type=E_COMPUTE_OPERATION_SUB;break;
					case '*':compiled_operation.operation_type=E_COMPUTE_OPERATION_MUL;break;
					case '/':compiled_operation.operation_type=E_COMPUTE_OPERATION_DIV;break;
					case '&':compiled_operation.operation_type=E_COMPUTE_OPERATION_AND;break;
					case '|':compiled_operation.operation_type=E_COMPUTE_OPERATION_OR;break;
					case '§':compiled_operation.operation_type=E_COMPUTE_OPERATION_MOD;break;
					case '^':compiled_operation.operation_type=E_COMPUTE_OPERATION_XOR;break;
					case 0:break;
				}
				ObjectArrayAddDynamicValueConcat((void **)&operation,&nboperation,&maxoperation,&compiled_operation,sizeof(compiled_operation));
				/* need to push again */
				if (lookforward) {
					lookforward=0;
					switch (operator1) {
						case '+':compiled_operation.operation_type=E_COMPUTE_OPERATION_ADD;break;
						case '-':compiled_operation.operation_type=E_COMPUTE_OPERATION_SUB;break;
						case '*':compiled_operation.operation_type=E_COMPUTE_OPERATION_MUL;break;
						case '/':compiled_operation.operation_type=E_COMPUTE_OPERATION_DIV;break;
						case '&':compiled_operation.operation_type=E_COMPUTE_OPERATION_AND;break;
						case '§':compiled_operation.operation_type=E_COMPUTE_OPERATION_MOD;break;
						case '|':compiled_operation.operation_type=E_COMPUTE_OPERATION_OR;break;
						case '^':compiled_operation.operation_type=E_COMPUTE_OPERATION_XOR;break;
					}
					ObjectArrayAddDynamicValueConcat((void **)&operation,&nboperation,&maxoperation,&compiled_operation,sizeof(compiled_operation));
				}
			}
		}
	
		/* we've just read a value, so we push it */
		if (ivar) {
			if (varbuffer[0]=='-') minusptr=1; else minusptr=0;
			/* constantes ou variables/labels */
			switch (varbuffer[minusptr]) {
				case '0':
				case '1':
				case '2':
				case '3':
				case '4':
				case '5':
				case '6':
				case '7':
				case '8':
				case '9':
					/* check number */
					for (icheck=minusptr;varbuffer[icheck];icheck++) {
						if ((varbuffer[icheck]>='0' && varbuffer[icheck]<='9') || varbuffer[icheck]=='.') continue;
						rasm_printf(ae,"[%s] Error line %d - expression [%s] - %s is not a valid number\n",GetExpFile(ae,didx),GetExpLine(ae,didx),zeexpression,varbuffer);
						MaxError(ae);
						break;
					}
					curval=atof(varbuffer+minusptr);
					break;
				case '%':
					/* check number */
					if (!varbuffer[minusptr+1]) {
						rasm_printf(ae,"[%s] Error line %d - expression [%s] - %s is an empty binary number\n",GetExpFile(ae,didx),GetExpLine(ae,didx),zeexpression,varbuffer);
						MaxError(ae);
					}
					for (icheck=minusptr+1;varbuffer[icheck];icheck++) {
						if (varbuffer[icheck]=='0' || varbuffer[icheck]=='1') continue;
						rasm_printf(ae,"[%s] Error line %d - expression [%s] - %s is not a valid binary number\n",GetExpFile(ae,didx),GetExpLine(ae,didx),zeexpression,varbuffer);
						MaxError(ae);
						break;
					}
					curval=strtol(varbuffer+minusptr+1,NULL,2);
					break;
				case '#':
					/* check number */
					if (!varbuffer[minusptr+1]) {
						rasm_printf(ae,"[%s] Error line %d - expression [%s] - %s is an empty hex number\n",GetExpFile(ae,didx),GetExpLine(ae,didx),zeexpression,varbuffer);
						MaxError(ae);
					}
					for (icheck=minusptr+1;varbuffer[icheck];icheck++) {
						if ((varbuffer[icheck]>='0' && varbuffer[icheck]<='9') || (varbuffer[icheck]>='A' && varbuffer[icheck]<='F')) continue;
						rasm_printf(ae,"[%s] Error line %d - expression [%s] - %s is not a valid hex number\n",GetExpFile(ae,didx),GetExpLine(ae,didx),zeexpression,varbuffer);
						MaxError(ae);
						break;
					}
					curval=strtol(varbuffer+minusptr+1,NULL,16);
					break;
				default:
                    crc=GetCRC(varbuffer+minusptr);
				
					for (imkey=0;math_keyword[imkey].mnemo[0];imkey++) {
						if (crc==math_keyword[imkey].crc && strcmp(varbuffer+minusptr,math_keyword[imkey].mnemo)==0) {
							if (c=='(') {
								if (isolation>=max_isolation_depth) {
									max_isolation_depth+=16;
									lookf_backup=MemRealloc(lookf_backup,sizeof(int)*max_isolation_depth);
									oper1_backup=MemRealloc(oper1_backup,sizeof(int)*max_isolation_depth);
									oper2_backup=MemRealloc(oper2_backup,sizeof(int)*max_isolation_depth);
									lowfl_backup=MemRealloc(lowfl_backup,sizeof(int)*max_isolation_depth);
									trigg_backup=MemRealloc(trigg_backup,sizeof(int)*max_isolation_depth);
									funct_backup=MemRealloc(funct_backup,sizeof(int)*max_isolation_depth);
								}
								lookf_backup[isolation]=lookforward;lookforward=0;
								oper1_backup[isolation]=operator1;
								oper2_backup[isolation]=operator2;
								lowfl_backup[isolation]=lowflag;lowflag=0;
								trigg_backup[isolation]=trigger;trigger=0;
								funct_backup[isolation]=math_keyword[imkey].operation;
								isolation++;
								allow_minus_as_sign=1;
								idx++;
							} else {
								rasm_printf(ae,"[%s] Error line %d - expression [%s] - %s is a reserved keyword!\n",GetExpFile(ae,didx),GetExpLine(ae,didx),zeexpression,math_keyword[imkey].mnemo);
								MaxError(ae);
								curval=0;
							}
							break;
						}
					}
					if (math_keyword[imkey].mnemo[0]) continue;
					
					if (varbuffer[minusptr+0]=='$' && varbuffer[minusptr+1]==0) {
						curval=ptr;
					} else {
						curdic=SearchDico(ae,varbuffer+minusptr,crc);
						if (curdic) {
							curval=curdic->v;
							break;
						} else {
							/* getbank hack */
							if (strncmp(varbuffer+minusptr,"{BANK}",6)==0) {
								bank=6;
								//printf("BANKSearch -> %s\n",varbuffer+minusptr+bank);
								/* obligé de recalculer le CRC */
								crc=GetCRC(varbuffer+minusptr+bank);
							} else {
								//printf("regular_label search [%s]\n",varbuffer+minusptr);
								bank=0;
							}
							/* limited label translation while processing crunched blocks
							   ae->curlz == current crunched block processed
							   expression->crunch_block=0 -> oui
							   expression->crunch_block=1 -> oui si même block
							   expression->crunch_block=2 -> non car sera relogée
							*/
							curlabel=SearchLabel(ae,varbuffer+minusptr+bank,crc);
							if (curlabel) {
								if (ae->stage<2) {
									if (curlabel->lz==-1) {
										if (bank) {
											curval=curlabel->ibank;
										} else {
											curval=curlabel->ptr;
										}
									} else {
										/* label MUST be in the crunched block */
										if (curlabel->iorgzone==ae->expression[didx].iorgzone && curlabel->ibank==ae->expression[didx].ibank && curlabel->lz<=ae->expression[didx].lz) {
											if (bank) {
												curval=curlabel->ibank;
											} else {
												curval=curlabel->ptr;
											}
											curval=curlabel->ptr;
										} else {
											rasm_printf(ae,"[%s] Error line %d - Label [%s](%d) cannot be computed because it is located after the crunched zone %d\n",GetExpFile(ae,didx),GetExpLine(ae,didx),varbuffer,curlabel->lz,ae->expression[didx].lz);
											MaxError(ae);
											curval=0;
										}
									}
								} else {
									if (bank) {
										curval=curlabel->ibank;
									} else {
										curval=curlabel->ptr;
									}
								}
							} else {
								/***********
									to allow aliases declared after use
								***********/
								if ((ialias=SearchAlias(ae,crc,varbuffer+minusptr))>=0) {
									newlen=ae->alias[ialias].len;
									lenw=strlen(zeexpression);
									if (newlen>ivar) {
										/* realloc bigger */
										if (original) {
											expr=MemMalloc(lenw+newlen-ivar+1);
											memcpy(expr,zeexpression,lenw+1);
											zeexpression=expr;
											original=0;
										} else {
											zeexpression=MemRealloc(zeexpression,lenw+newlen-ivar+1);
										}
									}
									/* startvar? */
									if (newlen!=ivar) {
										MemMove(zeexpression+startvar+newlen,zeexpression+startvar+ivar,lenw-startvar-ivar+1);
									}
									strncpy(zeexpression+startvar,ae->alias[ialias].translation,newlen); /* copy without zero terminator */
									idx=startvar;
									ivar=0;
									continue;
								} else {
									rasm_printf(ae,"[%s] Error line %d - in expression [%s]->[%s] (not found in variables,labels or aliases)\n",GetExpFile(ae,didx),GetExpLine(ae,didx),zeexpression,varbuffer+minusptr);
									MaxError(ae);
									curval=0;
								}
							}
						}
					}
			}
			if (minusptr) curval=-curval;

			if (!lowflag) {
				compiled_operation.operation_type=E_COMPUTE_OPERATION_PUSH_DATASTC;
				compiled_operation.data_static=curval;
				ObjectArrayAddDynamicValueConcat((void **)&operation,&nboperation,&maxoperation,&compiled_operation,sizeof(compiled_operation));
			}
			if (trigger) {
				if (lowflag) {
					curoperator=operator1;
					lookforward=1;
					operator1=operator2;
				} else {
					if (lookforward==2) {
						curoperator=operator2;
						lookforward=1;
					} else {
						curoperator=operator1;
						lookforward=0;
					}
				}
				switch (curoperator) {
					case '+':compiled_operation.operation_type=E_COMPUTE_OPERATION_ADD;break;
					case '-':compiled_operation.operation_type=E_COMPUTE_OPERATION_SUB;break;
					case '*':compiled_operation.operation_type=E_COMPUTE_OPERATION_MUL;break;
					case '/':compiled_operation.operation_type=E_COMPUTE_OPERATION_DIV;break;
					case '&':compiled_operation.operation_type=E_COMPUTE_OPERATION_AND;break;
					case '§':compiled_operation.operation_type=E_COMPUTE_OPERATION_MOD;break;
					case '|':compiled_operation.operation_type=E_COMPUTE_OPERATION_OR;break;
					case '^':compiled_operation.operation_type=E_COMPUTE_OPERATION_XOR;break;
				}
				ObjectArrayAddDynamicValueConcat((void **)&operation,&nboperation,&maxoperation,&compiled_operation,sizeof(compiled_operation));
				trigger=0;
			}
			if (lowflag) {
				lowflag=0;
				compiled_operation.operation_type=E_COMPUTE_OPERATION_PUSH_DATASTC;
				compiled_operation.data_static=curval;
				ObjectArrayAddDynamicValueConcat((void **)&operation,&nboperation,&maxoperation,&compiled_operation,sizeof(compiled_operation));
			}
			allow_minus_as_sign=0;
			ivar=0;
		}
	}

	/* duplicated bloc, to handle the last trigger, if any */
	while (lookforward) {
		if (lookforward==2) {
			curoperator=operator2;
			lookforward=1;
		} else {
			curoperator=operator1;
			lookforward=0;
		}
		switch (curoperator) {
			case '+':compiled_operation.operation_type=E_COMPUTE_OPERATION_ADD;break;
			case '-':compiled_operation.operation_type=E_COMPUTE_OPERATION_SUB;break;
			case '*':compiled_operation.operation_type=E_COMPUTE_OPERATION_MUL;break;
			case '/':compiled_operation.operation_type=E_COMPUTE_OPERATION_DIV;break;
			case '&':compiled_operation.operation_type=E_COMPUTE_OPERATION_AND;break;
			case '§':compiled_operation.operation_type=E_COMPUTE_OPERATION_MOD;break;
			case '|':compiled_operation.operation_type=E_COMPUTE_OPERATION_OR;break;
			case '^':compiled_operation.operation_type=E_COMPUTE_OPERATION_XOR;break;
		}
		ObjectArrayAddDynamicValueConcat((void **)&operation,&nboperation,&maxoperation,&compiled_operation,sizeof(compiled_operation));
		if (lookforward) {
			curoperator=operator1;
			lookforward=0;
			switch (curoperator) {
				case '&':compiled_operation.operation_type=E_COMPUTE_OPERATION_AND;break;
				case '|':compiled_operation.operation_type=E_COMPUTE_OPERATION_OR;break;
				case '^':compiled_operation.operation_type=E_COMPUTE_OPERATION_XOR;break;
				case '+':compiled_operation.operation_type=E_COMPUTE_OPERATION_ADD;break;
				case '-':compiled_operation.operation_type=E_COMPUTE_OPERATION_SUB;break;
				case '*':compiled_operation.operation_type=E_COMPUTE_OPERATION_MUL;break;
				case '/':compiled_operation.operation_type=E_COMPUTE_OPERATION_DIV;break;
				case '§':compiled_operation.operation_type=E_COMPUTE_OPERATION_MOD;break;
			}
			ObjectArrayAddDynamicValueConcat((void **)&operation,&nboperation,&maxoperation,&compiled_operation,sizeof(compiled_operation));
		}
		if (isolation) {
			isolation--;
			lookforward=lookf_backup[isolation];
			operator1=oper1_backup[isolation];
			operator2=oper2_backup[isolation];
			lowflag=lowfl_backup[isolation];
			trigger=trigg_backup[isolation];
		}
	}

	if (computing_error) {
		switch (computing_error) {
			case COMPUTING_OK:break;
			case COMPUTING_ERROR_CLOSING_PARENTHESIS:rasm_printf(ae,"(compute) cannot close parenthesis in %d because it was not opened before!\n",idx);break;
			case COMPUTING_ERROR_INVALID_CHARACTER:rasm_printf(ae,"(compute) invalid character\n",idx);break;
			case COMPUTING_ERROR_MINUS_WITHOUT_DATA:rasm_printf(ae,"(compute) numeric data or variable must follow operator+minus sign token!\n");break;
			default:rasm_printf(ae,"(compute) unknown error\n");break;
		}
		rasm_printf(ae,"[%s] Error line %d - invalid expression [%s]\n",GetExpFile(ae,didx),GetExpLine(ae,didx),zeexpression);
		MaxError(ae);
		if (!original) {
			MemFree(zeexpression);
		}
		return 0;
	}

	/* Execution des instructions */
	if (ae->maxam) {
		for (i=0;i<nboperation;i++) {

#if 0
			if (operation[i].operation_type==E_COMPUTE_OPERATION_PUSH_DATASTC) {
				printf("push %.1lf\n",operation[i].data_static);
			} else {
				printf("operation %s\n",operation[i].operation_type==E_COMPUTE_OPERATION_MUL?"*":operation[i].operation_type==E_COMPUTE_OPERATION_ADD?"+":"<autre>");
			}
#endif
			switch (operation[i].operation_type) {
				case E_COMPUTE_OPERATION_PUSH_DATASTC:
					if (maccu<=paccu) {
						maccu=16+paccu;
						accu=MemRealloc(accu,sizeof(double)*maccu);
					}
					accu[paccu]=operation[i].data_static;paccu++;
					break;
				case E_COMPUTE_OPERATION_ADD:if (paccu>1) accu[paccu-2]=((int)accu[paccu-2]+(int)accu[paccu-1])&0xFFFF;paccu--;break;
				case E_COMPUTE_OPERATION_SUB:if (paccu>1) accu[paccu-2]=((int)accu[paccu-2]-(int)accu[paccu-1])&0xFFFF;paccu--;break;
				case E_COMPUTE_OPERATION_MUL:if (paccu>1) accu[paccu-2]=((int)accu[paccu-2]*(int)accu[paccu-1])&0xFFFF;paccu--;break;
				case E_COMPUTE_OPERATION_DIV:if (paccu>1) accu[paccu-2]=((int)accu[paccu-2]/(int)accu[paccu-1])&0xFFFF;paccu--;break;
				case E_COMPUTE_OPERATION_AND:if (paccu>1) accu[paccu-2]=((int)accu[paccu-2]&(int)accu[paccu-1])&0xFFFF;paccu--;break;
				case E_COMPUTE_OPERATION_OR:if (paccu>1) accu[paccu-2]=((int)accu[paccu-2]|(int)accu[paccu-1])&0xFFFF;paccu--;break;
				case E_COMPUTE_OPERATION_XOR:if (paccu>1) accu[paccu-2]=((int)accu[paccu-2]^(int)accu[paccu-1])&0xFFFF;paccu--;break;
				case E_COMPUTE_OPERATION_MOD:if (paccu>1) accu[paccu-2]=((int)accu[paccu-2]%(int)accu[paccu-1])&0xFFFF;paccu--;break;
				case E_COMPUTE_OPERATION_SIN:if (paccu>0) accu[paccu-1]=(int)sin(accu[paccu-1]*3.1415926545/180.0);break;
				case E_COMPUTE_OPERATION_COS:if (paccu>0) accu[paccu-1]=(int)cos(accu[paccu-1]*3.1415926545/180.0);break;
				case E_COMPUTE_OPERATION_ASIN:if (paccu>0) accu[paccu-1]=(int)asin(accu[paccu-1])*180.0/3.1415926545;break;
				case E_COMPUTE_OPERATION_ACOS:if (paccu>0) accu[paccu-1]=(int)acos(accu[paccu-1])*180.0/3.1415926545;break;
				case E_COMPUTE_OPERATION_ATAN:if (paccu>0) accu[paccu-1]=(int)atan(accu[paccu-1])*180.0/3.1415926545;break;
				case E_COMPUTE_OPERATION_INT:break;
				case E_COMPUTE_OPERATION_FLOOR:if (paccu>0) accu[paccu-1]=(int)floor(accu[paccu-1])&0xFFFF;break;
				case E_COMPUTE_OPERATION_ABS:if (paccu>0) accu[paccu-1]=(int)fabs(accu[paccu-1])&0xFFFF;break;
				case E_COMPUTE_OPERATION_EXP:if (paccu>0) accu[paccu-1]=(int)exp(accu[paccu-1])&0xFFFF;break;
				case E_COMPUTE_OPERATION_LN:if (paccu>0) accu[paccu-1]=(int)log(accu[paccu-1])&0xFFFF;break;
				case E_COMPUTE_OPERATION_LOG10:if (paccu>0) accu[paccu-1]=(int)log10(accu[paccu-1])&0xFFFF;break;
				case E_COMPUTE_OPERATION_SQRT:if (paccu>0) accu[paccu-1]=(int)sqrt(accu[paccu-1])&0xFFFF;break;
				default:rasm_printf(ae,"invalid computing state! (%d)",operation[i].operation_type);
			}
			if (!paccu) {
				rasm_printf(ae,"[%s] Error line %d - empty stack with [%s]\n",GetExpFile(ae,didx),GetExpLine(ae,didx),zeexpression);
				exit(INTERNAL_ERROR);
			}
		}
	} else {
		for (i=0;i<nboperation;i++) {
#if 0
			if (operation[i].operation_type==E_COMPUTE_OPERATION_PUSH_DATASTC) {
				printf("push %.1lf\n",operation[i].data_static);
			} else {
				printf("operation %s\n",operation[i].operation_type==E_COMPUTE_OPERATION_MUL?"*":operation[i].operation_type==E_COMPUTE_OPERATION_ADD?"+":"<autre>");
			}
#endif
			switch (operation[i].operation_type) {
				case E_COMPUTE_OPERATION_PUSH_DATASTC:
					if (maccu<=paccu) {
						maccu=16+paccu;
						accu=MemRealloc(accu,sizeof(double)*maccu);
					}
					accu[paccu]=operation[i].data_static;paccu++;
					break;
				case E_COMPUTE_OPERATION_ADD:if (paccu>1) accu[paccu-2]+=accu[paccu-1];paccu--;break;
				case E_COMPUTE_OPERATION_SUB:if (paccu>1) accu[paccu-2]-=accu[paccu-1];paccu--;break;
				case E_COMPUTE_OPERATION_MUL:if (paccu>1) accu[paccu-2]*=accu[paccu-1];paccu--;break;
				case E_COMPUTE_OPERATION_DIV:if (paccu>1) accu[paccu-2]/=accu[paccu-1];paccu--;break;
				case E_COMPUTE_OPERATION_AND:if (paccu>1) accu[paccu-2]=((int)floor(accu[paccu-2]+0.5))&((int)floor(accu[paccu-1]+0.5));paccu--;break;
				case E_COMPUTE_OPERATION_OR:if (paccu>1) accu[paccu-2]=((int)floor(accu[paccu-2]+0.5))|((int)floor(accu[paccu-1]+0.5));paccu--;break;
				case E_COMPUTE_OPERATION_XOR:if (paccu>1) accu[paccu-2]=((int)floor(accu[paccu-2]+0.5))^((int)floor(accu[paccu-1]+0.5));paccu--;break;
				case E_COMPUTE_OPERATION_MOD:if (paccu>1) accu[paccu-2]=((int)floor(accu[paccu-2]+0.5))%((int)floor(accu[paccu-1]+0.5));paccu--;break;
				case E_COMPUTE_OPERATION_SIN:if (paccu>0) accu[paccu-1]=sin(accu[paccu-1]*3.1415926545/180.0);break;
				case E_COMPUTE_OPERATION_COS:if (paccu>0) accu[paccu-1]=cos(accu[paccu-1]*3.1415926545/180.0);break;
				case E_COMPUTE_OPERATION_ASIN:if (paccu>0) accu[paccu-1]=asin(accu[paccu-1])*180.0/3.1415926545;break;
				case E_COMPUTE_OPERATION_ACOS:if (paccu>0) accu[paccu-1]=acos(accu[paccu-1])*180.0/3.1415926545;break;
				case E_COMPUTE_OPERATION_ATAN:if (paccu>0) accu[paccu-1]=atan(accu[paccu-1])*180.0/3.1415926545;break;
				case E_COMPUTE_OPERATION_INT:if (paccu>0) accu[paccu-1]=floor(accu[paccu-1]+0.5);break;
				case E_COMPUTE_OPERATION_FLOOR:if (paccu>0) accu[paccu-1]=floor(accu[paccu-1]);break;
				case E_COMPUTE_OPERATION_ABS:if (paccu>0) accu[paccu-1]=fabs(accu[paccu-1]);break;
				case E_COMPUTE_OPERATION_EXP:if (paccu>0) accu[paccu-1]=exp(accu[paccu-1]);break;
				case E_COMPUTE_OPERATION_LN:if (paccu>0) accu[paccu-1]=log(accu[paccu-1]);break;
				case E_COMPUTE_OPERATION_LOG10:if (paccu>0) accu[paccu-1]=log10(accu[paccu-1]);break;
				case E_COMPUTE_OPERATION_SQRT:if (paccu>0) accu[paccu-1]=sqrt(accu[paccu-1]);break;
				default:rasm_printf(ae,"invalid computing state! (%d)",operation[i].operation_type);
			}
			if (!paccu) {
				rasm_printf(ae,"[%s] Error line %d - empty stack with [%s]",GetExpFile(ae,didx),GetExpLine(ae,didx),zeexpression);
				exit(INTERNAL_ERROR);
			}
		}
	}
	if (!original) {
		MemFree(zeexpression);
	}
	if (paccu==1) {
		return accu[0];
	} else {
		if (paccu) {
			rasm_printf(ae,"[%s] Error line %d - INTERNAL ERROR ComputeExpression stack still contains %d values!",GetExpFile(ae,didx),GetExpLine(ae,didx),paccu);
		} else {
			rasm_printf(ae,"[%s] Error line %d - INTERNAL ERROR ComputeExpression nothing in the stack",GetExpFile(ae,didx),GetExpLine(ae,didx));
		}
		return accu[paccu-1];
	}
}
int RoundComputeExpressionCore(struct s_assenv *ae,char *zeexpression,int ptr,int didx) {
	return floor(ComputeExpressionCore(ae,zeexpression,ptr,didx)+ae->rough);
}

void ExpressionSetDicoVar(struct s_assenv *ae,char *name, double v)
{
	#undef FUNC
	#define FUNC "ComputeExpression Core"

	struct s_expr_dico curdic;
	curdic.name=TxtStrDup(name);
	curdic.crc=GetCRC(name);
	curdic.v=v;
	//ObjectArrayAddDynamicValueConcat((void**)&ae->dico,&ae->idic,&ae->mdic,&curdic,sizeof(curdic));
	if (SearchLabel(ae,curdic.name,curdic.crc)) {
		rasm_printf(ae,"[%s] Error line %d - cannot create variable [%s] as there is already a label with the same name\n",GetExpFile(ae,0),GetExpLine(ae,0),name);
		MaxError(ae);
		MemFree(curdic.name);
		return;
	}
	InsertDicoToTree(ae,&curdic);
}
double ComputeExpression(struct s_assenv *ae,char *expr, int ptr, int didx, int expected_eval)
{
	#undef FUNC
	#define FUNC "ComputeExpression"

	char *ptr_exp,*ptr_exp2,backupeval;
	enum e_compute_evaluation_type wtd;
	int crc,idic,idx=0,ialias;
	double v,vl;
	struct s_alias curalias;
	struct s_expr_dico *curdic;

	while (!ae->AutomateExpressionDecision[((int)expr[idx])&0xFF]) idx++;

	switch (ae->AutomateExpressionDecision[((int)expr[idx])&0xFF]) {
		case '~':
			memset(&curalias,0,sizeof(curalias));
			ptr_exp=expr+idx;
			*ptr_exp=0;
			ptr_exp2=expr+idx+1;
			
			/* alias locaux */
			if (expr[0]=='@') {
				curalias.alias=MakeLocalLabel(ae,expr,NULL);
			} else {
				curalias.alias=TxtStrDup(expr);
			}
			curalias.crc=GetCRC(curalias.alias);
			if ((ialias=SearchAlias(ae,curalias.crc,curalias.alias))>=0) {
				rasm_printf(ae,"[%s] Error line %d - Duplicate alias [%s]\n",GetExpFile(ae,0),GetExpLine(ae,0),expr);
				MaxError(ae);
				MemFree(curalias.alias);
			} else {
				curalias.translation=MemMalloc(strlen(ptr_exp2)+1+2);
				sprintf(curalias.translation,"(%s)",ptr_exp2);
				ExpressionFastTranslate(ae,&curalias.translation,2);
				curalias.len=strlen(curalias.translation);
				ObjectArrayAddDynamicValueConcat((void**)&ae->alias,&ae->ialias,&ae->malias,&curalias,sizeof(curalias));
				CheckAndSortAliases(ae);
			}
			*ptr_exp='~';
			return 0;
		case '=':
			if (ae->AutomateExpressionDecision[((int)expr[idx+1])&0xFF]==0) {
				if (expected_eval) {
					if (ae->maxam) {
						/* maxam mode, force comparison */
						wtd=E_COMPUTE_EVALUATION_EQ;
						backupeval='=';
						ptr_exp=expr+idx;
						*ptr_exp=0;
						ptr_exp2=expr+idx+1;
					} else {
						rasm_printf(ae,"[%s] Error line %d - meaningless use of an expression [%s]\n",GetExpFile(ae,0),GetExpLine(ae,0),expr);
						MaxError(ae);
						return 0;
					}
				} else {
					/* affectation */
					if (expr[0]<'A' || expr[0]>'Z') {
						rasm_printf(ae,"[%s] Error line %d - variable name must begin by a letter [%s]\n",GetExpFile(ae,0),GetExpLine(ae,0),expr);
						MaxError(ae);
						return 0;
					} else {
						ptr_exp=expr+idx;
						v=ComputeExpressionCore(ae,ptr_exp+1,ptr,didx);
						*ptr_exp=0;
						crc=GetCRC(expr);
						curdic=SearchDico(ae,expr,crc);
						if (curdic) {
							/* on affecte */
							curdic->v=v;
						} else {
							/* on cree une nouvelle variable */
							ExpressionSetDicoVar(ae,expr,v);
						}
						*ptr_exp='=';
						return v;
					}
				}
			} else if (expr[idx+1]=='=') {
				/* egalite */
				wtd=E_COMPUTE_EVALUATION_EQ;
				backupeval='=';
				ptr_exp=expr+idx;
				*ptr_exp=0;
				ptr_exp2=expr+idx+2;
			} else {
				rasm_printf(ae,"[%s] Error line %d - Wrong expression\n",GetExpFile(ae,didx),GetExpLine(ae,didx));
				MaxError(ae);
				return 0;
			}
			break;
		case '>':
			if (expr[idx+1]=='=') {
				wtd=E_COMPUTE_EVALUATION_GE;
				backupeval='>';
				ptr_exp=expr+idx;
				*ptr_exp=0;
				ptr_exp2=expr+idx+2;
			} else {
				wtd=E_COMPUTE_EVALUATION_GT;
				backupeval='>';
				ptr_exp=expr+idx;
				*ptr_exp=0;
				ptr_exp2=expr+idx+1;
			}
			break;
		case '<':
			if (expr[idx+1]=='=') {
				wtd=E_COMPUTE_EVALUATION_LE;
				backupeval='<';
				ptr_exp=expr+idx;
				*ptr_exp=0;
				ptr_exp2=expr+idx+2;
			} else {
				wtd=E_COMPUTE_EVALUATION_LT;
				backupeval='<';
				ptr_exp=expr+idx;
				*ptr_exp=0;
				ptr_exp2=expr+idx+1;
			}
			break;
		case '!':
			if (expr[idx+1]=='=') {
				wtd=E_COMPUTE_EVALUATION_NE;
				backupeval='!';
				ptr_exp=expr+idx;
				*ptr_exp=0;
				ptr_exp2=expr+idx+2;
			} else {
				rasm_printf(ae,"[%s] Error line %d - Wrong expression\n",GetExpFile(ae,0),GetExpLine(ae,0));
				MaxError(ae);
				return 0;
			}
			break;
		case 'E':
			/* aucun opérateur de comparaison ou affectation rencontré, on calcule */
			if (expected_eval>1 && !ae->maxam) {
				rasm_printf(ae,"[%s] Error line %d - Infinite loop! [%s]\n",GetExpFile(ae,0),GetExpLine(ae,0),expr);
				MaxError(ae);
				return 0;
			} else {
				/* compute */
				v=ComputeExpressionCore(ae,expr,ptr,didx);
				return v;
			}
		default:
			rasm_printf(ae,"[%s] Error line %d - Internal ERROR - invalid evaluation state! [%s]\n",GetExpFile(ae,0),GetExpLine(ae,0),expr);
			exit(-34);
	}

	/* on compare expr et ptr_exp2 */
	vl=ComputeExpressionCore(ae,expr,ptr,didx);
	v=ComputeExpressionCore(ae,ptr_exp2,ptr,didx);
	*ptr_exp=backupeval;
	
	switch (wtd) {
		case E_COMPUTE_EVALUATION_LT:return vl<v;
		case E_COMPUTE_EVALUATION_LE:return vl<=v;
		case E_COMPUTE_EVALUATION_GT:return vl>v;
		case E_COMPUTE_EVALUATION_GE:return vl>=v;
		case E_COMPUTE_EVALUATION_NE:return vl!=v;
		case E_COMPUTE_EVALUATION_EQ:return vl==v;
		default:break;
	}
	return 0;
}
int RoundComputeExpression(struct s_assenv *ae,char *expr, int ptr, int didx, int expression_expected) {
	return floor(ComputeExpression(ae,expr,ptr,didx,expression_expected)+ae->rough);
}

void ExpressionFastTranslate(struct s_assenv *ae, char **ptr_expr, int fullreplace)
{
	#undef FUNC
	#define FUNC "ExpressionFastTranslate"

	struct s_label *curlabel;
	struct s_expr_dico *curdic;
	static char *varbuffer=NULL;
	static int ivar=0,maxivar=1;
	char curval[256]={0};
	int c,lenw=0,idx=0,crc,startvar,newlen,ialias,found_replace,yves,dek,reidx,lenbuf,rlen;
	double v;
	char tmpuchar[5];
	char *expr,*locallabel;
	
	if (!ae) {
		if (varbuffer) MemFree(varbuffer);
		varbuffer=NULL;
		maxivar=1;
		ivar=0;
		return;
	}
	/* be sure to have at least some bytes allocated */
	StateMachineResizeBuffer(&varbuffer,128,&maxivar);
	expr=*ptr_expr;

	while (!ae->AutomateExpressionDecision[((int)expr[idx])&0xFF]) idx++;

	switch (ae->maxam) {
		default:
		case 0: /* full check */
			if (expr[idx]=='~' || (expr[idx]=='=' && expr[idx+1]!='=')) {reidx=idx+1;break;}
			reidx=0;
			break;
		case 1: /* partial check with maxam */
			if (expr[idx]=='~') {reidx=idx+1;break;}
			reidx=0;
			break;
	}

	idx=0;
	/* is there ascii char? */
	while ((c=expr[idx])!=0) {
		if (c=='\'' || c=='"') {
			/* echappement */
			if (expr[idx+1]=='\\') {
				if (expr[idx+2] && expr[idx+3]==c) {
					sprintf(tmpuchar,"#%03X",expr[idx+2]);
					memcpy(expr+idx,tmpuchar,4);
					idx+=3;
				} else {
					rasm_printf(ae,"[%s] Error line %d - expression [%s] - Only single escaped char may be quoted\n",GetExpFile(ae,0),GetExpLine(ae,0),expr);
					MaxError(ae);
					expr[0]=0;
					return;
				}
			} else if (expr[idx+1] && expr[idx+2]==c) {
					sprintf(tmpuchar,"#%02X",(unsigned int)expr[idx+1]);
					memcpy(expr+idx,tmpuchar,3);
					idx+=2;
			} else {
				rasm_printf(ae,"[%s] Error line %d - expression [%s] - Only single char may be quoted\n",GetExpFile(ae,0),GetExpLine(ae,0),expr);
				MaxError(ae);
				expr[0]=0;
				return;
			}
		}
		idx++;
	}
	
	idx=reidx;
	while ((c=expr[idx])!=0) {
		switch (c) {
			/* operator / parenthesis */
			case '!':
			case '=':
			case '>':
			case '<':
			case '(':
			case ')':
			case ']':
			case '[':
			case '*':
			case '/':
			case '+':
			case '~':
			case '-':
			case '^':
			case '§':
			case '|':
			case '&':
				idx++;
				break;
			default:
				startvar=idx;
				while (ae->AutomateExpressionValidChar[((int)c)&0xFF]) {
					varbuffer[ivar++]=c;
					StateMachineResizeBuffer(&varbuffer,ivar,&maxivar);
					idx++;
					c=expr[idx];
				}
				varbuffer[ivar]=0;
				if (!ivar) {
					rasm_printf(ae,"[%s] Error line %d - invalid expression [%s]\n",GetExpFile(ae,0),GetExpLine(ae,0),expr);
					MaxError(ae);
					return;
				}
		}
		if (ivar) {
			/* recherche dans dictionnaire et remplacement */
			crc=GetCRC(varbuffer);
			found_replace=0;
			/* pour les affectations ou les tests conditionnels on ne remplace pas le dico (pour le Push oui par contre!) */
			if (fullreplace) {
				if (varbuffer[0]=='$' && !varbuffer[1]) {
					#ifdef OS_WIN
					snprintf(curval,sizeof(curval)-1,"%d",ae->codeadr);
					newlen=strlen(curval);
					#else
					newlen=snprintf(curval,sizeof(curval)-1,"%d",ae->codeadr);
					#endif
					lenw=strlen(expr);
					if (newlen>ivar) {
						/* realloc bigger */
						expr=*ptr_expr=MemRealloc(expr,lenw+newlen-ivar+1);
					}
					if (newlen!=ivar ) {
						MemMove(expr+startvar+newlen,expr+startvar+ivar,lenw-startvar-ivar+1);
						found_replace=1;
					}
					strncpy(expr+startvar,curval,newlen); /* copy without zero terminator */
					idx=startvar+newlen;
					ivar=0;
					found_replace=1;
				} else {
					curdic=SearchDico(ae,varbuffer,crc);
					if (curdic) {
						v=curdic->v;

						#ifdef OS_WIN
						snprintf(curval,sizeof(curval)-1,"%lf",v);
						newlen=strlen(curval);
						#else
						newlen=snprintf(curval,sizeof(curval)-1,"%lf",v);
						#endif
						lenw=strlen(expr);
						if (newlen>ivar) {
							/* realloc bigger */
							expr=*ptr_expr=MemRealloc(expr,lenw+newlen-ivar+1);
						}
						if (newlen!=ivar ) {
							MemMove(expr+startvar+newlen,expr+startvar+ivar,lenw-startvar-ivar+1);
						}
						strncpy(expr+startvar,curval,newlen); /* copy without zero terminator */
						idx=startvar+newlen;
						ivar=0;
						found_replace=1;
					}
				}
			}
			/* on cherche aussi dans les labels existants */
			if (!found_replace) {
				curlabel=SearchLabel(ae,varbuffer,crc);
				if (curlabel) {
					if (!curlabel->lz || ae->stage>1) {
						yves=curlabel->ptr;

						#ifdef OS_WIN
						snprintf(curval,sizeof(curval)-1,"%d",yves);
						newlen=strlen(curval);
						#else
						newlen=snprintf(curval,sizeof(curval)-1,"%d",yves);
						#endif
						lenw=strlen(expr);
						if (newlen>ivar) {
							/* realloc bigger */
							expr=*ptr_expr=MemRealloc(expr,lenw+newlen-ivar+1);
						}
						if (newlen!=ivar ) {
							MemMove(expr+startvar+newlen,expr+startvar+ivar,lenw-startvar-ivar+1);
							found_replace=1;
						}
						strncpy(expr+startvar,curval,newlen); /* copy without zero terminator */
						found_replace=1;
						idx=startvar+newlen;
						ivar=0;
					}
				}		
			}
			/* non trouve on cherche dans les alias */
			if (!found_replace) {
				if ((ialias=SearchAlias(ae,crc,varbuffer))>=0) {
					newlen=ae->alias[ialias].len;
					lenw=strlen(expr);
					/* controle de recursivite a l'insertion des alias */
					if (lenw>(32+ae->ialias+ae->nbword+ae->idic)*32) {
						rasm_printf(ae,"[%s] FATAL Error line %d - alias definition [%.30s] - infinite recursivity\n",GetExpFile(ae,0),GetExpLine(ae,0),expr);
						exit(551);
					}
					
					if (newlen>ivar) {
						/* realloc bigger */
						expr=*ptr_expr=MemRealloc(expr,lenw+newlen-ivar+1);
					}
					if (newlen!=ivar) {
						MemMove(expr+startvar+newlen,expr+startvar+ivar,lenw-startvar-ivar+1);
					}
					strncpy(expr+startvar,ae->alias[ialias].translation,newlen); /* copy without zero terminator */
					found_replace=1;
					idx=startvar;
					ivar=0;
				} else {
				}
			}
			/* non trouve c'est peut-etre un label local */
			if (!found_replace && varbuffer[0]=='@' && (ae->ir || ae->iw || ae->imacro)) {
				lenbuf=strlen(varbuffer);
				locallabel=MakeLocalLabel(ae,varbuffer,&dek);
				
				/*** le grand remplacement ***/
				rlen=strlen(expr+startvar+lenbuf)+1;
				expr=*ptr_expr=MemRealloc(expr,strlen(expr)+dek+1);
				MemMove(expr+startvar+lenbuf+dek,expr+startvar+lenbuf,rlen);
				strncpy(expr+startvar+lenbuf,locallabel,dek);
				idx+=dek;
				MemFree(locallabel);
				found_replace=1;
			}
			ivar=0;
		}
	}
}

void PushExpression(struct s_assenv *ae,int iw,enum e_expression zetype)
{
	#undef FUNC
	#define FUNC "PushExpression"
	
	struct s_expression curexp={0};
	int startptr=0;

	curexp.iw=iw;
	curexp.wptr=ae->outputadr;
	curexp.zetype=zetype;
	curexp.ibank=ae->activebank;
	curexp.iorgzone=ae->io-1;
	curexp.lz=ae->lz;
	/* on traduit de suite les variables du dictionnaire pour les boucles et increments
		SAUF si c'est une affectation */
	if (!ae->wl[iw].e) {
		switch (zetype) {
			case E_EXPRESSION_J8:
			case E_EXPRESSION_V8:
			case E_EXPRESSION_V16:
			case E_EXPRESSION_IM:startptr=-1;
						break;
			case E_EXPRESSION_IV8:
			case E_EXPRESSION_IV16:startptr=-2;
						break;
			case E_EXPRESSION_3V8:startptr=-3;
						break;
			default:break;
		}
		/* hack pourri pour gérer le $ */
		ae->codeadr+=startptr;
		/* ok mais les labels locaux des macros? */
		if (ae->ir || ae->iw || ae->imacro) {
			curexp.reference=TxtStrDup(ae->wl[iw].w);
			ExpressionFastTranslate(ae,&curexp.reference,1);
		} else {
			ExpressionFastTranslate(ae,&ae->wl[iw].w,1);
		}
		ae->codeadr-=startptr;
	}
	/* calcul adresse de rÃ©fÃ©rence et post-incrÃ©mentation pour sauter les data */
	switch (zetype) {
		case E_EXPRESSION_J8:curexp.ptr=ae->codeadr-1;ae->outputadr++;ae->codeadr++;break;
		case E_EXPRESSION_0V8:curexp.ptr=ae->codeadr;ae->outputadr++;ae->codeadr++;break;
		case E_EXPRESSION_V8:curexp.ptr=ae->codeadr-1;ae->outputadr++;ae->codeadr++;break;
		case E_EXPRESSION_0V16:curexp.ptr=ae->codeadr;ae->outputadr+=2;ae->codeadr+=2;break;
		case E_EXPRESSION_0V32:curexp.ptr=ae->codeadr;ae->outputadr+=4;ae->codeadr+=4;break;
		case E_EXPRESSION_V16:curexp.ptr=ae->codeadr-1;ae->outputadr+=2;ae->codeadr+=2;break;
		case E_EXPRESSION_IV8:curexp.ptr=ae->codeadr-2;ae->outputadr++;ae->codeadr++;break;
		case E_EXPRESSION_3V8:curexp.ptr=ae->codeadr-3;ae->outputadr++;ae->codeadr++;break;
		case E_EXPRESSION_IV16:curexp.ptr=ae->codeadr-2;ae->outputadr+=2;ae->codeadr+=2;break;
		case E_EXPRESSION_RST:curexp.ptr=ae->codeadr;ae->outputadr++;ae->codeadr++;break;
		case E_EXPRESSION_IM:curexp.ptr=ae->codeadr-1;ae->outputadr++;ae->codeadr++;break;
	}	
	if (ae->outputadr<=ae->maxptr) {
		ObjectArrayAddDynamicValueConcat((void **)&ae->expression,&ae->ie,&ae->me,&curexp,sizeof(curexp));
	} else {
		rasm_printf(ae,"[%s] Error line %d - output exceed limit %d\n",GetCurrentFile(ae),ae->wl[ae->idx].l,ae->maxptr);
		exit(3);
	}
}

void PopAllSave(struct s_assenv *ae)
{
	#undef FUNC
	#define FUNC "PopAllSave"
	
	char *filename;
	int offset,size;
	int is;
	
	for (is=0;is<ae->nbsave;is++) {
		/* avoid quotes */
		filename=ae->wl[ae->save[is].iw].w;
		filename[strlen(filename)-1]=0;
		filename++;
		/**/
		ae->idx=ae->save[is].ioffset; /* exp hack */
		ExpressionFastTranslate(ae,&ae->wl[ae->idx].w,0);
		offset=RoundComputeExpression(ae,ae->wl[ae->save[is].ioffset].w,0,0,0);
		ExpressionFastTranslate(ae,&ae->wl[ae->save[is].isize].w,0);
		size=RoundComputeExpression(ae,ae->wl[ae->save[is].isize].w,0,0,0);
		rasm_printf(ae,"Write binary file %s (%d byte%s)\n",filename,size,size>1?"s":"");
		FileRemoveIfExists(filename);
		FileWriteBinary(filename,(char*)ae->mem[ae->save[is].ibank]+offset,size);
		FileWriteBinaryClose(filename);
	}
	if (ae->nbsave) {
		MemFree(ae->save);
	}
}

void PopAllExpression(struct s_assenv *ae, int crunched_zone)
{
	#undef FUNC
	#define FUNC "PopAllExpression"
	
	static int first=1;
	double v;
	long r;
	int i;

	unsigned char *mem;
	char *expr;
	
	/* pop all expressions BUT thoses who where already computed (in crunched blocks) */

	/* calcul des labels et expressions en zone crunch (et locale?)
	   les labels doivent pointer:
	   - une valeur absolue (numerique ou variable calculee) -> completement transparent
	   - un label dans la meme zone de crunch -> label->lz=1 && verif de la zone crunch
	   - un label hors zone crunch MAIS avant toute zone de crunch de la bank destination (!label->lz)

	   idealement on doit tolerer les adresses situees apres le crunch dans une autre ORG zone!

	   on utilise ae->stage pour créer un état intermédiaire dans le ComputeExpressionCore
	*/
	if (crunched_zone>=0) {
		ae->stage=1;
	} else {
		/* on rescanne tout pour combler les trous */
		ae->stage=2;
		first=1;
	}
	
	for (i=first;i<ae->ie;i++) {
		/* first compute only crunched expression (0,1,2,3,...) then (-1) at the end */
		if (crunched_zone>=0) {
			/* calcul des expressions en zone crunch */
			if (ae->expression[i].lz<crunched_zone) continue;
			if (ae->expression[i].lz>crunched_zone) {
				first=i;
				break;
			}
		} else {
			if (ae->expression[i].lz>=0) continue;
		}

		mem=ae->mem[ae->expression[i].ibank];
		
		if (ae->expression[i].reference) {
			expr=ae->expression[i].reference;
		} else {
			expr=ae->wl[ae->expression[i].iw].w;
		}
		v=ComputeExpressionCore(ae,expr,ae->expression[i].ptr,i);
		r=(long)floor(v+ae->rough);
		switch (ae->expression[i].zetype) {
			case E_EXPRESSION_J8:
				r=r-ae->expression[i].ptr-2;
				if (r<-128 || r>127) {
					rasm_printf(ae,"[%s] Error line %d - relative offset %d too far [%s]\n",GetExpFile(ae,i),ae->wl[ae->expression[i].iw].l,r,ae->wl[ae->expression[i].iw].w);
					MaxError(ae);
				}
				mem[ae->expression[i].wptr]=(unsigned char)r;
				break;
			case E_EXPRESSION_0V8:
			case E_EXPRESSION_IV8:
			case E_EXPRESSION_3V8:
			case E_EXPRESSION_V8:
				if (r>255 || r<-128) {
					rasm_printf(ae,"[%s] Warning line %d - truncating value #%X to #%X\n",GetExpFile(ae,i),ae->wl[ae->expression[i].iw].l,r,r&0xFF);
				}
				mem[ae->expression[i].wptr]=(unsigned char)r;
				break;
			case E_EXPRESSION_IV16:
			case E_EXPRESSION_V16:
			case E_EXPRESSION_0V16:
				if (r>65535 || r<-32768) {
					rasm_printf(ae,"[%s] Warning line %d - truncating value #%X to #%X\n",GetExpFile(ae,i),ae->wl[ae->expression[i].iw].l,r,r&0xFFFF);
				}
				mem[ae->expression[i].wptr]=(unsigned char)r&0xFF;
				mem[ae->expression[i].wptr+1]=(unsigned char)((r>>8)&0xFF);
				break;
			case E_EXPRESSION_0V32:
				/* meaningless in 32 bits architecture... */
				if (v>4294967295 || v<-2147483648) {
					rasm_printf(ae,"[%s] Warning line %d - truncating value\n",GetExpFile(ae,i),ae->wl[ae->expression[i].iw].l);
				}
				mem[ae->expression[i].wptr]=(unsigned char)r&0xFF;
				mem[ae->expression[i].wptr+1]=(unsigned char)((r>>8)&0xFF);
				mem[ae->expression[i].wptr+2]=(unsigned char)((r>>16)&0xFF);
				mem[ae->expression[i].wptr+3]=(unsigned char)((r>>24)&0xFF);
				break;
			case E_EXPRESSION_IM:
				switch (r) {
					case 0x00:mem[ae->expression[i].wptr]=0x46;break;
					case 0x01:mem[ae->expression[i].wptr]=0x56;break;
					case 0x02:mem[ae->expression[i].wptr]=0x5E;break;
					default:
						rasm_printf(ae,"[%s] Error line %d - IM 0,1 or 2 only\n",GetExpFile(ae,i),ae->wl[ae->expression[i].iw].l);
						MaxError(ae);
						mem[ae->expression[i].wptr]=0;
				}
				break;
			case E_EXPRESSION_RST:
				switch (r) {
					case 0x00:mem[ae->expression[i].wptr]=0xC7;break;
					case 0x08:mem[ae->expression[i].wptr]=0xCF;break;
					case 0x10:mem[ae->expression[i].wptr]=0xD7;break;
					case 0x18:mem[ae->expression[i].wptr]=0xDF;break;
					case 0x20:mem[ae->expression[i].wptr]=0xE7;break;
					case 0x28:mem[ae->expression[i].wptr]=0xEF;break;
					case 0x30:mem[ae->expression[i].wptr]=0xF7;break;
					case 0x38:mem[ae->expression[i].wptr]=0xFF;break;
					default:
						rasm_printf(ae,"[%s] Error line %d - RST #0,#8,#10,#18,#20,#28,#30,#38 only\n",GetExpFile(ae,i),ae->wl[ae->expression[i].iw].l);
						MaxError(ae);
						mem[ae->expression[i].wptr]=0;
				}
				break;
			default:
				rasm_printf(ae,"[%s] FATAL Error line %d - unknown expression type\n",GetExpFile(ae,i),ae->wl[ae->expression[i].iw].l);
				exit(-8);
		}	
	}
}

void InsertLabelToTree(struct s_assenv *ae, struct s_label *label)
{
	#undef FUNC
	#define FUNC "InsertLabelToTree"

	struct s_crclabel_tree *curlabeltree;
	int radix,dek=32;

	curlabeltree=&ae->labeltree;
	while (dek) {
		dek=dek-8;
		radix=(label->crc>>dek)&0xFF;
		if (curlabeltree->radix[radix]) {
			curlabeltree=curlabeltree->radix[radix];
		} else {
			curlabeltree->radix[radix]=MemMalloc(sizeof(struct s_crclabel_tree));
			curlabeltree=curlabeltree->radix[radix];
			memset(curlabeltree,0,sizeof(struct s_crclabel_tree));
		}
	}
	ObjectArrayAddDynamicValueConcat((void**)&curlabeltree->label,&curlabeltree->nlabel,&curlabeltree->mlabel,label,sizeof(struct s_label));
}

void PushLabel(struct s_assenv *ae)
{
	#undef FUNC
	#define FUNC "PushLabel"
	
	struct s_label curlabel={0},*searched_label;
	char *curlabelname;
	int i;

	if (ae->AutomateValidLabelFirst[ae->wl[ae->idx].w[0]]) {
		for (i=1;ae->wl[ae->idx].w[i];i++) {
			if (!ae->AutomateValidLabel[ae->wl[ae->idx].w[i]]) {
				rasm_printf(ae,"[%s] Error line %d -  Invalid char in label declaration\n",GetExpFile(ae,0),ae->wl[ae->idx].l);
				MaxError(ae);
				return;
			}
		}
	} else {
		rasm_printf(ae,"[%s] Error line %d -  Invalid first char in label declaration\n",GetExpFile(ae,0),ae->wl[ae->idx].l);
		MaxError(ae);
		return;
	}
	switch (i) {
		case 1:
			switch (ae->wl[ae->idx].w[0]) {
				case 'A':
				case 'B':
				case 'C':
				case 'D':
				case 'E':
				case 'F':
				case 'H':
				case 'L':
				case 'I':
				case 'R':
					rasm_printf(ae,"[%s] Error line %d - Cannot use reserved word for label\n",GetExpFile(ae,0),ae->wl[ae->idx].l);
					MaxError(ae);
					return;
				default:break;
			}
			break;
		case 2:
			if (strcmp(ae->wl[ae->idx].w,"AF")==0 || strcmp(ae->wl[ae->idx].w,"BC")==0 || strcmp(ae->wl[ae->idx].w,"DE")==0 || strcmp(ae->wl[ae->idx].w,"HL")==0 || 
				strcmp(ae->wl[ae->idx].w,"IX")==0 || strcmp(ae->wl[ae->idx].w,"IY")==0 || strcmp(ae->wl[ae->idx].w,"SP")==0 ||
				strcmp(ae->wl[ae->idx].w,"LX")==0 || strcmp(ae->wl[ae->idx].w,"HX")==0 || strcmp(ae->wl[ae->idx].w,"XL")==0 || strcmp(ae->wl[ae->idx].w,"XH")==0 ||
				strcmp(ae->wl[ae->idx].w,"LY")==0 || strcmp(ae->wl[ae->idx].w,"HY")==0 || strcmp(ae->wl[ae->idx].w,"YL")==0 || strcmp(ae->wl[ae->idx].w,"YH")==0) {
				rasm_printf(ae,"[%s] Error line %d - Cannot use reserved word for label\n",GetExpFile(ae,0),ae->wl[ae->idx].l);
				MaxError(ae);
				return;
			}
			break;
		case 3:
			if (strcmp(ae->wl[ae->idx].w,"IXL")==0 || strcmp(ae->wl[ae->idx].w,"IYL")==0 || strcmp(ae->wl[ae->idx].w,"IXH")==0 || strcmp(ae->wl[ae->idx].w,"IYH")==0) {
				rasm_printf(ae,"[%s] Error line %d - Cannot use reserved word for label\n",GetExpFile(ae,0),ae->wl[ae->idx].l);
				MaxError(ae);
				return;
			}
			break;
		default:break;
	}
	
	/* labels locaux */
	if (ae->wl[ae->idx].w[0]=='@' && (ae->ir || ae->iw || ae->imacro)) {
		
		curlabel.iw=-1;
		curlabelname=curlabel.name=MakeLocalLabel(ae,ae->wl[ae->idx].w,NULL);
		curlabel.crc=GetCRC(curlabel.name);
	} else {
		/* ancien style */
		if (ae->wl[ae->idx].w[0]=='.') {
			i=0;
			do {
				ae->wl[ae->idx].w[i]=ae->wl[ae->idx].w[i+1];
				i++;
			} while (ae->wl[ae->idx].w[i]!=0);
		}
		curlabel.iw=ae->idx;
		curlabel.crc=GetCRC(ae->wl[ae->idx].w);
		curlabelname=ae->wl[ae->idx].w;

		/* contrôle dico uniquement avec des labels non locaux */
		if (SearchDico(ae,curlabelname,curlabel.crc)) {
			rasm_printf(ae,"[%s] Error line %d - cannot create label [%s] as there is already a variable with the same name\n",GetExpFile(ae,0),ae->wl[ae->idx].l,curlabelname);
			MaxError(ae);
			return;
		}
	}
	curlabel.ptr=ae->codeadr;
	curlabel.ibank=ae->activebank;
	curlabel.iorgzone=ae->io-1;
	curlabel.lz=ae->lz;

	if ((searched_label=SearchLabel(ae,curlabelname,curlabel.crc))!=NULL) {
		rasm_printf(ae,"[%s] Error line %d - Duplicate label [%s]\n",GetExpFile(ae,0),GetExpLine(ae,0),curlabelname);
		MaxError(ae);
		if (curlabel.iw==-1) MemFree(curlabelname);
	} else {
		ObjectArrayAddDynamicValueConcat((void **)&ae->label,&ae->il,&ae->ml,&curlabel,sizeof(curlabel));
		InsertLabelToTree(ae,&curlabel);
	}
}


#undef FUNC
#define FUNC "Instruction CORE"

void _IN(struct s_assenv *ae) {
	if (!ae->wl[ae->idx].t && !ae->wl[ae->idx+1].t && ae->wl[ae->idx+2].t==1) {
		if (strcmp(ae->wl[ae->idx+2].w,"(C)")==0) {
			switch (GetCRC(ae->wl[ae->idx+1].w)) {
				case CRC_0:
				case CRC_F:___output(ae,0xED);___output(ae,0x70);break;
				case CRC_A:___output(ae,0xED);___output(ae,0x78);break;
				case CRC_B:___output(ae,0xED);___output(ae,0x40);break;
				case CRC_C:___output(ae,0xED);___output(ae,0x48);break;
				case CRC_D:___output(ae,0xED);___output(ae,0x50);break;
				case CRC_E:___output(ae,0xED);___output(ae,0x58);break;
				case CRC_H:___output(ae,0xED);___output(ae,0x60);break;
				case CRC_L:___output(ae,0xED);___output(ae,0x68);break;
				default:
					rasm_printf(ae,"[%s] Error line %d -  syntax is IN [0,F,A,B,C,D,E,H,L],(C)\n",GetExpFile(ae,0),ae->wl[ae->idx].l);
					MaxError(ae);
			}
		} else if (strcmp(ae->wl[ae->idx+1].w,"A")==0 && StringIsMem(ae->wl[ae->idx+2].w)) {
			___output(ae,0xDB);
			PushExpression(ae,ae->idx+2,E_EXPRESSION_V8);
		} else {
			rasm_printf(ae,"[%s] Error line %d -  IN [0,F,A,B,C,D,E,H,L],(C) or IN A,(n) only\n",GetExpFile(ae,0),ae->wl[ae->idx].l);
			MaxError(ae);
		}
		ae->idx+=2;
	} else {
		rasm_printf(ae,"[%s] Error line %d -  IN [0,F,A,B,C,D,E,H,L],(C) or IN A,(n) only\n",GetExpFile(ae,0),ae->wl[ae->idx].l);
		MaxError(ae);
	}
}

void _OUT(struct s_assenv *ae) {
	if (!ae->wl[ae->idx].t && !ae->wl[ae->idx+1].t && ae->wl[ae->idx+2].t==1) {
		if (strcmp(ae->wl[ae->idx+1].w,"(C)")==0) {
			switch (GetCRC(ae->wl[ae->idx+2].w)) {
				case CRC_0:___output(ae,0xED);___output(ae,0x71);break;
				case CRC_A:___output(ae,0xED);___output(ae,0x79);break;
				case CRC_B:___output(ae,0xED);___output(ae,0x41);break;
				case CRC_C:___output(ae,0xED);___output(ae,0x49);break;
				case CRC_D:___output(ae,0xED);___output(ae,0x51);break;
				case CRC_E:___output(ae,0xED);___output(ae,0x59);break;
				case CRC_H:___output(ae,0xED);___output(ae,0x61);break;
				case CRC_L:___output(ae,0xED);___output(ae,0x69);break;
				default:
					rasm_printf(ae,"[%s] Error line %d -  syntax is OUT (C),[A,B,C,D,E,H,L]\n",GetExpFile(ae,0),ae->wl[ae->idx].l);
					MaxError(ae);
			}
		} else if (strcmp(ae->wl[ae->idx+2].w,"A")==0 && StringIsMem(ae->wl[ae->idx+1].w)) {
			___output(ae,0xD3);
			PushExpression(ae,ae->idx+1,E_EXPRESSION_V8);
		} else {
			rasm_printf(ae,"[%s] Error line %d -  OUT (C),[A,B,C,D,E,H,L] or OUT (n),A only\n",GetExpFile(ae,0),ae->wl[ae->idx].l);
			MaxError(ae);
		}
		ae->idx+=2;
	} else {
		rasm_printf(ae,"[%s] Error line %d -  OUT (C),[A,B,C,D,E,H,L] or OUT (n),A only\n",GetExpFile(ae,0),ae->wl[ae->idx].l);
		MaxError(ae);
	}
}

void _EX(struct s_assenv *ae) {
	if (!ae->wl[ae->idx].t && !ae->wl[ae->idx+1].t && ae->wl[ae->idx+2].t==1) {
		switch (GetCRC(ae->wl[ae->idx+1].w)) {
			case CRC_HL:
				switch (GetCRC(ae->wl[ae->idx+2].w)) {
					case CRC_DE:___output(ae,0xEB);break;
					case CRC_SP:___output(ae,0xE3);break;
					default:
						rasm_printf(ae,"[%s] Error line %d -  syntax is EX HL,[SP,DE]\n",GetExpFile(ae,0),ae->wl[ae->idx].l);
						MaxError(ae);
				}
				break;
			case CRC_AF:
				if (strcmp(ae->wl[ae->idx+2].w,"AF'")==0) {
					___output(ae,0x08);
				} else {
					rasm_printf(ae,"[%s] Error line %d -  syntax is EX AF,AF'\n",GetExpFile(ae,0),ae->wl[ae->idx].l);
					MaxError(ae);
				}
				break;
			case CRC_MSP:
				switch (GetCRC(ae->wl[ae->idx+2].w)) {
					case CRC_HL:___output(ae,0xE3);break;
					case CRC_IX:___output(ae,0xDD);___output(ae,0xE3);break;
					case CRC_IY:___output(ae,0xFD);___output(ae,0xE3);break;
					default:
						rasm_printf(ae,"[%s] Error line %d -  syntax is EX SP,[HL,IX,IY]\n",GetExpFile(ae,0),ae->wl[ae->idx].l);
						MaxError(ae);
				}
				break;
			case CRC_DE:
				switch (GetCRC(ae->wl[ae->idx+2].w)) {
					case CRC_HL:___output(ae,0xEB);break;
					default:
						rasm_printf(ae,"[%s] Error line %d -  syntax is EX DE,HL\n",GetExpFile(ae,0),ae->wl[ae->idx].l);
						MaxError(ae);
				}
				break;
			case CRC_IX:
				switch (GetCRC(ae->wl[ae->idx+2].w)) {
					case CRC_SP:___output(ae,0xDD);___output(ae,0xE3);break;
					default:
						rasm_printf(ae,"[%s] Error line %d -  syntax is EX IX,SP\n",GetExpFile(ae,0),ae->wl[ae->idx].l);
						MaxError(ae);
				}
				break;
			case CRC_IY:
				switch (GetCRC(ae->wl[ae->idx+2].w)) {
					case CRC_SP:___output(ae,0xFD);___output(ae,0xE3);break;
					default:
						rasm_printf(ae,"[%s] Error line %d -  syntax is EX IY,SP\n",GetExpFile(ae,0),ae->wl[ae->idx].l);
						MaxError(ae);
				}
				break;
			default:
				rasm_printf(ae,"[%s] Error line %d -  syntax is EX [AF,DE,HL,SP,IX,IY],reg16\n",GetExpFile(ae,0),ae->wl[ae->idx].l);
				MaxError(ae);
		}
		ae->idx+=2;
	} else {
		rasm_printf(ae,"[%s] Error line %d -  Use EX reg16,reg16\n",GetExpFile(ae,0),ae->wl[ae->idx].l);
		MaxError(ae);
	}
}

void _SBC(struct s_assenv *ae) {
	if (!ae->wl[ae->idx].t && ae->wl[ae->idx+1].t==1 || (!ae->wl[ae->idx].t && !ae->wl[ae->idx+1].t && ae->wl[ae->idx+2].t==1) && strcmp(ae->wl[ae->idx+1].w,"A")==0) {
		if (!ae->wl[ae->idx+1].t) ae->idx++;
		switch (GetCRC(ae->wl[ae->idx+1].w)) {
			case CRC_A:___output(ae,0x9F);break;
			case CRC_MHL:___output(ae,0x9E);break;
			case CRC_B:___output(ae,0x98);break;
			case CRC_C:___output(ae,0x99);break;
			case CRC_D:___output(ae,0x9A);break;
			case CRC_E:___output(ae,0x9B);break;
			case CRC_H:___output(ae,0x9C);break;
			case CRC_L:___output(ae,0x9D);break;
			case CRC_IXH:case CRC_HX:case CRC_XH:___output(ae,0xDD);___output(ae,0x9C);break;
			case CRC_IXL:case CRC_LX:case CRC_XL:___output(ae,0xDD);___output(ae,0x9D);break;
			case CRC_IYH:case CRC_HY:case CRC_YH:___output(ae,0xFD);___output(ae,0x9C);break;
			case CRC_IYL:case CRC_LY:case CRC_YL:___output(ae,0xFD);___output(ae,0x9D);break;
			default:
				if (strncmp(ae->wl[ae->idx+1].w,"(IX",3)==0) {
					___output(ae,0xDD);___output(ae,0x9E);
					PushExpression(ae,ae->idx+1,E_EXPRESSION_IV8);
				} else if (strncmp(ae->wl[ae->idx+1].w,"(IY",3)==0) {
					___output(ae,0xFD);___output(ae,0x9E);
					PushExpression(ae,ae->idx+1,E_EXPRESSION_IV8);
				} else {
					___output(ae,0xDE);
					PushExpression(ae,ae->idx+1,E_EXPRESSION_V8);
				}
		}
		ae->idx++;
	} else if (!ae->wl[ae->idx].t && !ae->wl[ae->idx+1].t && ae->wl[ae->idx+2].t==1) {
		switch (GetCRC(ae->wl[ae->idx+1].w)) {
			case CRC_HL:
				switch (GetCRC(ae->wl[ae->idx+2].w)) {
					case CRC_BC:___output(ae,0xED);___output(ae,0x42);break;
					case CRC_DE:___output(ae,0xED);___output(ae,0x52);break;
					case CRC_HL:___output(ae,0xED);___output(ae,0x62);break;
					case CRC_SP:___output(ae,0xED);___output(ae,0x72);break;
					default:
						rasm_printf(ae,"[%s] Error line %d -  syntax is SBC HL,[BC,DE,HL,SP]\n",GetExpFile(ae,0),ae->wl[ae->idx].l);
						MaxError(ae);
				}
				break;
			default:
				rasm_printf(ae,"[%s] Error line %d -  syntax is SBC HL,[BC,DE,HL,SP]\n",GetExpFile(ae,0),ae->wl[ae->idx].l);
				MaxError(ae);
		}
		ae->idx+=2;
	} else {
		rasm_printf(ae,"[%s] Error line %d -  Invalid syntax for SBC\n",GetExpFile(ae,0),ae->wl[ae->idx].l);
		MaxError(ae);
	}
}

void _ADC(struct s_assenv *ae) {
	if (!ae->wl[ae->idx].t && ae->wl[ae->idx+1].t==1 || (!ae->wl[ae->idx].t && !ae->wl[ae->idx+1].t && ae->wl[ae->idx+2].t==1) && strcmp(ae->wl[ae->idx+1].w,"A")==0) {
		if (!ae->wl[ae->idx+1].t) ae->idx++;
		switch (GetCRC(ae->wl[ae->idx+1].w)) {
			case CRC_A:___output(ae,0x8F);break;
			case CRC_MHL:___output(ae,0x8E);break;
			case CRC_B:___output(ae,0x88);break;
			case CRC_C:___output(ae,0x89);break;
			case CRC_D:___output(ae,0x8A);break;
			case CRC_E:___output(ae,0x8B);break;
			case CRC_H:___output(ae,0x8C);break;
			case CRC_L:___output(ae,0x8D);break;
			case CRC_IXH:case CRC_HX:case CRC_XH:___output(ae,0xDD);___output(ae,0x8C);break;
			case CRC_IXL:case CRC_LX:case CRC_XL:___output(ae,0xDD);___output(ae,0x8D);break;
			case CRC_IYH:case CRC_HY:case CRC_YH:___output(ae,0xFD);___output(ae,0x8C);break;
			case CRC_IYL:case CRC_LY:case CRC_YL:___output(ae,0xFD);___output(ae,0x8D);break;
			default:
				if (strncmp(ae->wl[ae->idx+1].w,"(IX",3)==0) {
					___output(ae,0xDD);___output(ae,0x8E);
					PushExpression(ae,ae->idx+1,E_EXPRESSION_IV8);
				} else if (strncmp(ae->wl[ae->idx+1].w,"(IY",3)==0) {
					___output(ae,0xFD);___output(ae,0x8E);
					PushExpression(ae,ae->idx+1,E_EXPRESSION_IV8);
				} else {
					___output(ae,0xCE);
					PushExpression(ae,ae->idx+1,E_EXPRESSION_V8);
				}
		}
		ae->idx++;
	} else if (!ae->wl[ae->idx].t && !ae->wl[ae->idx+1].t && ae->wl[ae->idx+2].t==1) {
		switch (GetCRC(ae->wl[ae->idx+1].w)) {
			case CRC_HL:
				switch (GetCRC(ae->wl[ae->idx+2].w)) {
					case CRC_BC:___output(ae,0xED);___output(ae,0x4A);break;
					case CRC_DE:___output(ae,0xED);___output(ae,0x5A);break;
					case CRC_HL:___output(ae,0xED);___output(ae,0x6A);break;
					case CRC_SP:___output(ae,0xED);___output(ae,0x7A);break;
					default:
						rasm_printf(ae,"[%s] Error line %d -  syntax is ADC HL,[BC,DE,HL,SP]\n",GetExpFile(ae,0),ae->wl[ae->idx].l);
						MaxError(ae);
				}
				break;
			default:
				rasm_printf(ae,"[%s] Error line %d -  syntax is ADC HL,[BC,DE,HL,SP]\n",GetExpFile(ae,0),ae->wl[ae->idx].l);
				MaxError(ae);
		}
		ae->idx+=2;
	} else {
		rasm_printf(ae,"[%s] Error line %d -  Invalid syntax for ADC\n",GetExpFile(ae,0),ae->wl[ae->idx].l);
		MaxError(ae);
	}
}

void _ADD(struct s_assenv *ae) {
	if (!ae->wl[ae->idx].t && ae->wl[ae->idx+1].t==1 || (!ae->wl[ae->idx].t && !ae->wl[ae->idx+1].t && ae->wl[ae->idx+2].t==1) && strcmp(ae->wl[ae->idx+1].w,"A")==0) {
		if (!ae->wl[ae->idx+1].t) ae->idx++;
		switch (GetCRC(ae->wl[ae->idx+1].w)) {
			case CRC_A:___output(ae,0x87);break;
			case CRC_MHL:___output(ae,0x86);break;
			case CRC_B:___output(ae,0x80);break;
			case CRC_C:___output(ae,0x81);break;
			case CRC_D:___output(ae,0x82);break;
			case CRC_E:___output(ae,0x83);break;
			case CRC_H:___output(ae,0x84);break;
			case CRC_L:___output(ae,0x85);break;
			case CRC_IXH:case CRC_HX:case CRC_XH:___output(ae,0xDD);___output(ae,0x84);break;
			case CRC_IXL:case CRC_LX:case CRC_XL:___output(ae,0xDD);___output(ae,0x85);break;
			case CRC_IYH:case CRC_HY:case CRC_YH:___output(ae,0xFD);___output(ae,0x84);break;
			case CRC_IYL:case CRC_LY:case CRC_YL:___output(ae,0xFD);___output(ae,0x85);break;
			default:
				if (strncmp(ae->wl[ae->idx+1].w,"(IX",3)==0) {
					___output(ae,0xDD);___output(ae,0x86);
					PushExpression(ae,ae->idx+1,E_EXPRESSION_IV8);
				} else if (strncmp(ae->wl[ae->idx+1].w,"(IY",3)==0) {
					___output(ae,0xFD);___output(ae,0x86);
					PushExpression(ae,ae->idx+1,E_EXPRESSION_IV8);
				} else {
					___output(ae,0xC6);
					PushExpression(ae,ae->idx+1,E_EXPRESSION_V8);
				}
		}
		ae->idx++;
	} else if (!ae->wl[ae->idx].t && !ae->wl[ae->idx+1].t && ae->wl[ae->idx+2].t==1) {
		switch (GetCRC(ae->wl[ae->idx+1].w)) {
			case CRC_HL:
				switch (GetCRC(ae->wl[ae->idx+2].w)) {
					case CRC_BC:___output(ae,0x09);break;
					case CRC_DE:___output(ae,0x19);break;
					case CRC_HL:___output(ae,0x29);break;
					case CRC_SP:___output(ae,0x39);break;
					default:
						rasm_printf(ae,"[%s] Error line %d -  syntax is ADD HL,[BC,DE,HL,SP]\n",GetExpFile(ae,0),ae->wl[ae->idx].l);
						MaxError(ae);
				}
				break;
			case CRC_IX:
				switch (GetCRC(ae->wl[ae->idx+2].w)) {
					case CRC_BC:___output(ae,0xDD);___output(ae,0x09);break;
					case CRC_DE:___output(ae,0xDD);___output(ae,0x19);break;
					case CRC_IX:___output(ae,0xDD);___output(ae,0x29);break;
					case CRC_SP:___output(ae,0xDD);___output(ae,0x39);break;
					default:
						rasm_printf(ae,"[%s] Error line %d -  syntax is ADD IX,[BC,DE,IX,SP]\n",GetExpFile(ae,0),ae->wl[ae->idx].l);
						MaxError(ae);
				}
				break;
			case CRC_IY:
				switch (GetCRC(ae->wl[ae->idx+2].w)) {
					case CRC_BC:___output(ae,0xFD);___output(ae,0x09);break;
					case CRC_DE:___output(ae,0xFD);___output(ae,0x19);break;
					case CRC_IY:___output(ae,0xFD);___output(ae,0x29);break;
					case CRC_SP:___output(ae,0xFD);___output(ae,0x39);break;
					default:
						rasm_printf(ae,"[%s] Error line %d -  syntax is ADD IY,[BC,DE,IY,SP]\n",GetExpFile(ae,0),ae->wl[ae->idx].l);
						MaxError(ae);
				}
				break;
			default:
				rasm_printf(ae,"[%s] Error line %d -  syntax is ADD [HL,IX,IY],reg16\n",GetExpFile(ae,0),ae->wl[ae->idx].l);
				MaxError(ae);
		}
		ae->idx+=2;
	} else {
		rasm_printf(ae,"[%s] Error line %d -  Invalid syntax for ADD\n",GetExpFile(ae,0),ae->wl[ae->idx].l);
		MaxError(ae);
	}
}

void _CP(struct s_assenv *ae) {
	if (!ae->wl[ae->idx].t && ae->wl[ae->idx+1].t==1) {
		switch (GetCRC(ae->wl[ae->idx+1].w)) {
			case CRC_A:___output(ae,0xBF);break;
			case CRC_MHL:___output(ae,0xBE);break;
			case CRC_B:___output(ae,0xB8);break;
			case CRC_C:___output(ae,0xB9);break;
			case CRC_D:___output(ae,0xBA);break;
			case CRC_E:___output(ae,0xBB);break;
			case CRC_H:___output(ae,0xBC);break;
			case CRC_L:___output(ae,0xBD);break;
			case CRC_IXH:case CRC_HX:case CRC_XH:___output(ae,0xDD);___output(ae,0xBC);break;
			case CRC_IXL:case CRC_LX:case CRC_XL:___output(ae,0xDD);___output(ae,0xBD);break;
			case CRC_IYH:case CRC_HY:case CRC_YH:___output(ae,0xFD);___output(ae,0xBC);break;
			case CRC_IYL:case CRC_LY:case CRC_YL:___output(ae,0xFD);___output(ae,0xBD);break;
			default:
				if (strncmp(ae->wl[ae->idx+1].w,"(IX",3)==0) {
					___output(ae,0xDD);___output(ae,0xBE);
					PushExpression(ae,ae->idx+1,E_EXPRESSION_IV8);
				} else if (strncmp(ae->wl[ae->idx+1].w,"(IY",3)==0) {
					___output(ae,0xFD);___output(ae,0xBE);
					PushExpression(ae,ae->idx+1,E_EXPRESSION_IV8);
				} else {
					___output(ae,0xFE);
					PushExpression(ae,ae->idx+1,E_EXPRESSION_V8);
				}
		}
		ae->idx++;
	} else {
		rasm_printf(ae,"[%s] Error line %d -  Syntax is CP reg8/(reg16)\n",GetExpFile(ae,0),ae->wl[ae->idx].l);
		MaxError(ae);
	}
}

void _RET(struct s_assenv *ae) {
	if (!ae->wl[ae->idx].t && ae->wl[ae->idx+1].t==1) {
		switch (GetCRC(ae->wl[ae->idx+1].w)) {
			case CRC_NZ:___output(ae,0xC0);break;
			case CRC_Z:___output(ae,0xC8);break;
			case CRC_C:___output(ae,0xD8);break;
			case CRC_NC:___output(ae,0xD0);break;
			case CRC_PE:___output(ae,0xE8);break;
			case CRC_PO:___output(ae,0xE0);break;
			case CRC_P:___output(ae,0xF0);break;
			case CRC_M:___output(ae,0xF8);break;
			default:
				rasm_printf(ae,"[%s] Error line %d -  Available flags for RET are C,NC,Z,NZ,PE,PO,P,M\n",GetExpFile(ae,0),ae->wl[ae->idx].l);
				MaxError(ae);
		}
		ae->idx++;
	} else if (ae->wl[ae->idx].t==1) {
		___output(ae,0xC9);
	} else {
		rasm_printf(ae,"[%s] Error line %d -  Invalid RET syntax\n",GetExpFile(ae,0),ae->wl[ae->idx].l);
		MaxError(ae);
	}
}

void _CALL(struct s_assenv *ae) {
	if (!ae->wl[ae->idx].t && ae->wl[ae->idx+1].t==0 && ae->wl[ae->idx+2].t==1) {
		switch (GetCRC(ae->wl[ae->idx+1].w)) {
			case CRC_C:___output(ae,0xDC);break;
			case CRC_Z:___output(ae,0xCC);break;
			case CRC_NZ:___output(ae,0xC4);break;
			case CRC_NC:___output(ae,0xD4);break;
			case CRC_PE:___output(ae,0xEC);break;
			case CRC_PO:___output(ae,0xE4);break;
			case CRC_P:___output(ae,0xF4);break;
			case CRC_M:___output(ae,0xFC);break;
			default:
				rasm_printf(ae,"[%s] Error line %d -  Available flags for CALL are C,NC,Z,NZ,PE,PO,P,M\n",GetExpFile(ae,0),ae->wl[ae->idx].l);
				MaxError(ae);
		}
		PushExpression(ae,ae->idx+2,E_EXPRESSION_V16);
		ae->idx+=2;
	} else if (!ae->wl[ae->idx].t && ae->wl[ae->idx+1].t==1) {
		___output(ae,0xCD);
		PushExpression(ae,ae->idx+1,E_EXPRESSION_V16);
		ae->idx++;
	} else {
		rasm_printf(ae,"[%s] Error line %d -  Invalid CALL syntax\n",GetExpFile(ae,0),ae->wl[ae->idx].l);
		MaxError(ae);
	}
}

void _JR(struct s_assenv *ae) {
	if (!ae->wl[ae->idx].t && ae->wl[ae->idx+1].t==0 && ae->wl[ae->idx+2].t==1) {
		switch (GetCRC(ae->wl[ae->idx+1].w)) {
			case CRC_NZ:___output(ae,0x20);break;
			case CRC_C:___output(ae,0x38);break;
			case CRC_Z:___output(ae,0x28);break;
			case CRC_NC:___output(ae,0x30);break;
			default:
				rasm_printf(ae,"[%s] Error line %d -  Available flags for JR are C,NC,Z,NZ\n",GetExpFile(ae,0),ae->wl[ae->idx].l);
				MaxError(ae);
		}
		PushExpression(ae,ae->idx+2,E_EXPRESSION_J8);
		ae->idx+=2;
	} else if (!ae->wl[ae->idx].t && ae->wl[ae->idx+1].t==1) {
		___output(ae,0x18);
		PushExpression(ae,ae->idx+1,E_EXPRESSION_J8);
		ae->idx++;
	} else {
		rasm_printf(ae,"[%s] Error line %d -  Invalid JR syntax\n",GetExpFile(ae,0),ae->wl[ae->idx].l);
		MaxError(ae);
	}
}

void _JP(struct s_assenv *ae) {
	if (!ae->wl[ae->idx].t && ae->wl[ae->idx+1].t==0 && ae->wl[ae->idx+2].t==1) {
		switch (GetCRC(ae->wl[ae->idx+1].w)) {
			case CRC_C:___output(ae,0xDA);break;
			case CRC_Z:___output(ae,0xCA);break;
			case CRC_NZ:___output(ae,0xC2);break;
			case CRC_NC:___output(ae,0xD2);break;
			case CRC_PE:___output(ae,0xEA);break;
			case CRC_PO:___output(ae,0xE2);break;
			case CRC_P:___output(ae,0xF2);break;
			case CRC_M:___output(ae,0xFA);break;
			default:
				rasm_printf(ae,"[%s] Error line %d -  Available flags for JP are C,NC,Z,NZ,PE,PO,P,M\n",GetExpFile(ae,0),ae->wl[ae->idx].l);
				MaxError(ae);
		}
		PushExpression(ae,ae->idx+2,E_EXPRESSION_V16);
		ae->idx+=2;
	} else if (!ae->wl[ae->idx].t && ae->wl[ae->idx+1].t==1) {
		switch (GetCRC(ae->wl[ae->idx+1].w)) {
			case CRC_HL:case CRC_MHL:___output(ae,0xE9);break;
			case CRC_IX:case CRC_MIX:___output(ae,0xDD);___output(ae,0xE9);break;
			case CRC_IY:case CRC_MIY:___output(ae,0xFD);___output(ae,0xE9);break;
			default:
				___output(ae,0xC3);
				PushExpression(ae,ae->idx+1,E_EXPRESSION_V16);
		}
		ae->idx++;
	} else {
		rasm_printf(ae,"[%s] Error line %d -  Invalid JP syntax\n",GetExpFile(ae,0),ae->wl[ae->idx].l);
		MaxError(ae);
	}
}


void _DEC(struct s_assenv *ae) {
	if (!ae->wl[ae->idx].t && ae->wl[ae->idx+1].t==1) {
		switch (GetCRC(ae->wl[ae->idx+1].w)) {
			case CRC_A:___output(ae,0x3D);break;
			case CRC_B:___output(ae,0x05);break;
			case CRC_C:___output(ae,0x0D);break;
			case CRC_D:___output(ae,0x15);break;
			case CRC_E:___output(ae,0x1D);break;
			case CRC_H:___output(ae,0x25);break;
			case CRC_L:___output(ae,0x2D);break;
			case CRC_IXH:case CRC_HX:case CRC_XH:___output(ae,0xDD);___output(ae,0x25);break;
			case CRC_IXL:case CRC_LX:case CRC_XL:___output(ae,0xDD);___output(ae,0x2D);break;
			case CRC_IYH:case CRC_HY:case CRC_YH:___output(ae,0xFD);___output(ae,0x25);break;
			case CRC_IYL:case CRC_LY:case CRC_YL:___output(ae,0xFD);___output(ae,0x2D);break;
			case CRC_BC:___output(ae,0x0B);break;
			case CRC_DE:___output(ae,0x1B);break;
			case CRC_HL:___output(ae,0x2B);break;
			case CRC_IX:___output(ae,0xDD);___output(ae,0x2B);break;
			case CRC_IY:___output(ae,0xFD);___output(ae,0x2B);break;
			case CRC_SP:___output(ae,0x3B);break;
			case CRC_MHL:___output(ae,0x35);break;
			default:
				if (strncmp(ae->wl[ae->idx+1].w,"(IX",3)==0) {
					___output(ae,0xDD);___output(ae,0x35);
					PushExpression(ae,ae->idx+1,E_EXPRESSION_IV8);
				} else if (strncmp(ae->wl[ae->idx+1].w,"(IY",3)==0) {
					___output(ae,0xFD);___output(ae,0x35);
					PushExpression(ae,ae->idx+1,E_EXPRESSION_IV8);
				} else {
					rasm_printf(ae,"[%s] Error line %d -  Use DEC with A,B,C,D,E,H,L,XH,XL,YH,YL,BC,DE,HL,SP,(HL),(IX),(IY)\n",GetExpFile(ae,0),ae->wl[ae->idx].l);
					MaxError(ae);
				}
		}
		ae->idx++;
	} else {
		rasm_printf(ae,"[%s] Error line %d -  Use DEC with A,B,C,D,E,H,L,XH,XL,YH,YL,BC,DE,HL,SP,(HL),(IX),(IY)\n",GetExpFile(ae,0),ae->wl[ae->idx].l);
		MaxError(ae);
	}
}
void _INC(struct s_assenv *ae) {
	if (!ae->wl[ae->idx].t && ae->wl[ae->idx+1].t==1) {
		switch (GetCRC(ae->wl[ae->idx+1].w)) {
			case CRC_A:___output(ae,0x3C);break;
			case CRC_B:___output(ae,0x04);break;
			case CRC_C:___output(ae,0x0C);break;
			case CRC_D:___output(ae,0x14);break;
			case CRC_E:___output(ae,0x1C);break;
			case CRC_H:___output(ae,0x24);break;
			case CRC_L:___output(ae,0x2C);break;
			case CRC_IXH:case CRC_HX:case CRC_XH:___output(ae,0xDD);___output(ae,0x24);break;
			case CRC_IXL:case CRC_LX:case CRC_XL:___output(ae,0xDD);___output(ae,0x2C);break;
			case CRC_IYH:case CRC_HY:case CRC_YH:___output(ae,0xFD);___output(ae,0x24);break;
			case CRC_IYL:case CRC_LY:case CRC_YL:___output(ae,0xFD);___output(ae,0x2C);break;
			case CRC_BC:___output(ae,0x03);break;
			case CRC_DE:___output(ae,0x13);break;
			case CRC_HL:___output(ae,0x23);break;
			case CRC_IX:___output(ae,0xDD);___output(ae,0x23);break;
			case CRC_IY:___output(ae,0xFD);___output(ae,0x23);break;
			case CRC_SP:___output(ae,0x33);break;
			case CRC_MHL:___output(ae,0x34);break;
			default:
				if (strncmp(ae->wl[ae->idx+1].w,"(IX",3)==0) {
					___output(ae,0xDD);___output(ae,0x34);
					PushExpression(ae,ae->idx+1,E_EXPRESSION_IV8);
				} else if (strncmp(ae->wl[ae->idx+1].w,"(IY",3)==0) {
					___output(ae,0xFD);___output(ae,0x34);
					PushExpression(ae,ae->idx+1,E_EXPRESSION_IV8);
				} else {
					rasm_printf(ae,"[%s] Error line %d -  Use INC with A,B,C,D,E,H,L,XH,XL,YH,YL,BC,DE,HL,SP,(HL),(IX),(IY)\n",GetExpFile(ae,0),ae->wl[ae->idx].l);
					MaxError(ae);
				}
		}
		ae->idx++;
	} else {
		rasm_printf(ae,"[%s] Error line %d -  Use INC with A,B,C,D,E,H,L,XH,XL,YH,YL,BC,DE,HL,SP,(HL),(IX),(IY)\n",GetExpFile(ae,0),ae->wl[ae->idx].l);
		MaxError(ae);
	}
}

void _SUB(struct s_assenv *ae) {
	#ifdef OPCODE
	#undef OPCODE
	#endif
	#define OPCODE 0x90
	
	if (!ae->wl[ae->idx].t && ae->wl[ae->idx+1].t==1  || (!ae->wl[ae->idx].t && !ae->wl[ae->idx+1].t && ae->wl[ae->idx+2].t==1) && strcmp(ae->wl[ae->idx+1].w,"A")==0) {
		if (!ae->wl[ae->idx+1].t) ae->idx++;
		switch (GetCRC(ae->wl[ae->idx+1].w)) {
			case CRC_A:___output(ae,OPCODE+7);break;
			case CRC_MHL:___output(ae,OPCODE+6);break;
			case CRC_B:___output(ae,OPCODE);break;
			case CRC_C:___output(ae,OPCODE+1);break;
			case CRC_D:___output(ae,OPCODE+2);break;
			case CRC_E:___output(ae,OPCODE+3);break;
			case CRC_H:___output(ae,OPCODE+4);break;
			case CRC_L:___output(ae,OPCODE+5);break;
			case CRC_IXH:case CRC_HX:case CRC_XH:___output(ae,0xDD);___output(ae,OPCODE+4);break;
			case CRC_IXL:case CRC_LX:case CRC_XL:___output(ae,0xDD);___output(ae,OPCODE+5);break;
			case CRC_IYH:case CRC_HY:case CRC_YH:___output(ae,0xFD);___output(ae,OPCODE+4);break;
			case CRC_IYL:case CRC_LY:case CRC_YL:___output(ae,0xFD);___output(ae,OPCODE+5);break;
			default:
				if (strncmp(ae->wl[ae->idx+1].w,"(IX",3)==0) {
					___output(ae,0xDD);___output(ae,OPCODE+6);
					PushExpression(ae,ae->idx+1,E_EXPRESSION_IV8);
				} else if (strncmp(ae->wl[ae->idx+1].w,"(IY",3)==0) {
					___output(ae,0xFD);___output(ae,OPCODE+6);
					PushExpression(ae,ae->idx+1,E_EXPRESSION_IV8);
				} else {
					___output(ae,0xD6);
					PushExpression(ae,ae->idx+1,E_EXPRESSION_V8);
				}
		}
		ae->idx++;
	} else {
		rasm_printf(ae,"[%s] Error line %d -  Use SUB with A,B,C,D,E,H,L,XH,XL,YH,YL,(HL),(IX),(IY)\n",GetExpFile(ae,0),ae->wl[ae->idx].l);
		MaxError(ae);
	}
}
void _AND(struct s_assenv *ae) {
	#ifdef OPCODE
	#undef OPCODE
	#endif
	#define OPCODE 0xA0
	
	if (!ae->wl[ae->idx].t && ae->wl[ae->idx+1].t==1) {
		switch (GetCRC(ae->wl[ae->idx+1].w)) {
			case CRC_A:___output(ae,OPCODE+7);break;
			case CRC_MHL:___output(ae,OPCODE+6);break;
			case CRC_B:___output(ae,OPCODE);break;
			case CRC_C:___output(ae,OPCODE+1);break;
			case CRC_D:___output(ae,OPCODE+2);break;
			case CRC_E:___output(ae,OPCODE+3);break;
			case CRC_H:___output(ae,OPCODE+4);break;
			case CRC_L:___output(ae,OPCODE+5);break;
			case CRC_IXH:case CRC_HX:case CRC_XH:___output(ae,0xDD);___output(ae,OPCODE+4);break;
			case CRC_IXL:case CRC_LX:case CRC_XL:___output(ae,0xDD);___output(ae,OPCODE+5);break;
			case CRC_IYH:case CRC_HY:case CRC_YH:___output(ae,0xFD);___output(ae,OPCODE+4);break;
			case CRC_IYL:case CRC_LY:case CRC_YL:___output(ae,0xFD);___output(ae,OPCODE+5);break;
			default:
				if (strncmp(ae->wl[ae->idx+1].w,"(IX",3)==0) {
					___output(ae,0xDD);___output(ae,OPCODE+6);
					PushExpression(ae,ae->idx+1,E_EXPRESSION_IV8);
				} else if (strncmp(ae->wl[ae->idx+1].w,"(IY",3)==0) {
					___output(ae,0xFD);___output(ae,OPCODE+6);
					PushExpression(ae,ae->idx+1,E_EXPRESSION_IV8);
				} else {
					___output(ae,0xE6);
					PushExpression(ae,ae->idx+1,E_EXPRESSION_V8);
				}
		}
		ae->idx++;
	} else {
		rasm_printf(ae,"[%s] Error line %d - Use AND with A,B,C,D,E,H,L,XH,XL,YH,YL,(HL),(IX),(IY)\n",GetExpFile(ae,0),ae->wl[ae->idx].l);
		MaxError(ae);
	}
}
void _OR(struct s_assenv *ae) {
	#ifdef OPCODE
	#undef OPCODE
	#endif
	#define OPCODE 0xB0
	
	if (!ae->wl[ae->idx].t && ae->wl[ae->idx+1].t==1) {
		switch (GetCRC(ae->wl[ae->idx+1].w)) {
			case CRC_A:___output(ae,OPCODE+7);break;
			case CRC_MHL:___output(ae,OPCODE+6);break;
			case CRC_B:___output(ae,OPCODE);break;
			case CRC_C:___output(ae,OPCODE+1);break;
			case CRC_D:___output(ae,OPCODE+2);break;
			case CRC_E:___output(ae,OPCODE+3);break;
			case CRC_H:___output(ae,OPCODE+4);break;
			case CRC_L:___output(ae,OPCODE+5);break;
			case CRC_IXH:case CRC_HX:case CRC_XH:___output(ae,0xDD);___output(ae,OPCODE+4);break;
			case CRC_IXL:case CRC_LX:case CRC_XL:___output(ae,0xDD);___output(ae,OPCODE+5);break;
			case CRC_IYH:case CRC_HY:case CRC_YH:___output(ae,0xFD);___output(ae,OPCODE+4);break;
			case CRC_IYL:case CRC_LY:case CRC_YL:___output(ae,0xFD);___output(ae,OPCODE+5);break;
			default:
				if (strncmp(ae->wl[ae->idx+1].w,"(IX",3)==0) {
					___output(ae,0xDD);___output(ae,OPCODE+6);
					PushExpression(ae,ae->idx+1,E_EXPRESSION_IV8);
				} else if (strncmp(ae->wl[ae->idx+1].w,"(IY",3)==0) {
					___output(ae,0xFD);___output(ae,OPCODE+6);
					PushExpression(ae,ae->idx+1,E_EXPRESSION_IV8);
				} else {
					___output(ae,0xF6);
					PushExpression(ae,ae->idx+1,E_EXPRESSION_V8);
				}
		}
		ae->idx++;
	} else {
		rasm_printf(ae,"[%s] Error line %d -  Use OR with A,B,C,D,E,H,L,XH,XL,YH,YL,(HL),(IX),(IY)\n",GetExpFile(ae,0),ae->wl[ae->idx].l);
		MaxError(ae);
	}
}
void _XOR(struct s_assenv *ae) {
	#ifdef OPCODE
	#undef OPCODE
	#endif
	#define OPCODE 0xA8
	
	if (!ae->wl[ae->idx].t && ae->wl[ae->idx+1].t==1) {
		switch (GetCRC(ae->wl[ae->idx+1].w)) {
			case CRC_A:___output(ae,OPCODE+7);break;
			case CRC_MHL:___output(ae,OPCODE+6);break;
			case CRC_B:___output(ae,OPCODE);break;
			case CRC_C:___output(ae,OPCODE+1);break;
			case CRC_D:___output(ae,OPCODE+2);break;
			case CRC_E:___output(ae,OPCODE+3);break;
			case CRC_H:___output(ae,OPCODE+4);break;
			case CRC_L:___output(ae,OPCODE+5);break;
			case CRC_IXH:case CRC_HX:case CRC_XH:___output(ae,0xDD);___output(ae,OPCODE+4);break;
			case CRC_IXL:case CRC_LX:case CRC_XL:___output(ae,0xDD);___output(ae,OPCODE+5);break;
			case CRC_IYH:case CRC_HY:case CRC_YH:___output(ae,0xFD);___output(ae,OPCODE+4);break;
			case CRC_IYL:case CRC_LY:case CRC_YL:___output(ae,0xFD);___output(ae,OPCODE+5);break;
			default:
				if (strncmp(ae->wl[ae->idx+1].w,"(IX",3)==0) {
					___output(ae,0xDD);___output(ae,OPCODE+6);
					PushExpression(ae,ae->idx+1,E_EXPRESSION_IV8);
				} else if (strncmp(ae->wl[ae->idx+1].w,"(IY",3)==0) {
					___output(ae,0xFD);___output(ae,OPCODE+6);
					PushExpression(ae,ae->idx+1,E_EXPRESSION_IV8);
				} else {
					___output(ae,0xEE);
					PushExpression(ae,ae->idx+1,E_EXPRESSION_V8);
				}
		}
		ae->idx++;
	} else {
		rasm_printf(ae,"[%s] Error line %d -  Use XOR with A,B,C,D,E,H,L,XH,XL,YH,YL,(HL),(IX),(IY)\n",GetCurrentFile(ae),ae->wl[ae->idx].l);
		MaxError(ae);
	}
}


void _POP(struct s_assenv *ae) {
	if (!ae->wl[ae->idx].t && ae->wl[ae->idx+1].t==1) {
		switch (GetCRC(ae->wl[ae->idx+1].w)) {
			case CRC_AF:___output(ae,0xF1);break;
			case CRC_BC:___output(ae,0xC1);break;
			case CRC_DE:___output(ae,0xD1);break;
			case CRC_HL:___output(ae,0xE1);break;
			case CRC_IX:___output(ae,0xDD);___output(ae,0xE1);break;
			case CRC_IY:___output(ae,0xFD);___output(ae,0xE1);break;
			default:
				rasm_printf(ae,"Error line %d - Use POP with AF,BC,DE,HL,IX,IY\n",GetCurrentFile(ae),ae->wl[ae->idx].l);
				MaxError(ae);
		}
		ae->idx++;
	} else {
		rasm_printf(ae,"[%s] Error line %d - POP need one parameter\n",ae->wl[ae->idx].l);
		MaxError(ae);
	}
}
void _PUSH(struct s_assenv *ae) {
	if (!ae->wl[ae->idx].t && ae->wl[ae->idx+1].t==1) {
		switch (GetCRC(ae->wl[ae->idx+1].w)) {
			case CRC_AF:___output(ae,0xF5);break;
			case CRC_BC:___output(ae,0xC5);break;
			case CRC_DE:___output(ae,0xD5);break;
			case CRC_HL:___output(ae,0xE5);break;
			case CRC_IX:___output(ae,0xDD);___output(ae,0xE5);break;
			case CRC_IY:___output(ae,0xFD);___output(ae,0xE5);break;
			default:
				rasm_printf(ae,"Error line %d - Use PUSH with AF,BC,DE,HL,IX,IY\n",ae->wl[ae->idx].l);
				MaxError(ae);
		}
		ae->idx++;
	} else {
		rasm_printf(ae,"[%s] Error line %d - PUSH need one parameter\n",GetCurrentFile(ae),ae->wl[ae->idx].l);
		MaxError(ae);
	}
}

void _IM(struct s_assenv *ae) {
	if (!ae->wl[ae->idx].t && ae->wl[ae->idx+1].t==1) {
		/* la valeur du paramÃ¨tre va dÃ©finir l'opcode du IM */
		___output(ae,0xED);
		PushExpression(ae,ae->idx+1,E_EXPRESSION_IM);
		ae->idx++;
	} else {
		rasm_printf(ae,"[%s] Error line %d - IM need one parameter\n",GetCurrentFile(ae),ae->wl[ae->idx].l);
		MaxError(ae);
	}
}

void _RLCA(struct s_assenv *ae) {
	if (ae->wl[ae->idx].t) {
		___output(ae,0x7);
	} else {
		rasm_printf(ae,"[%s] Error line %d - RLCA does not need parameter\n",GetCurrentFile(ae),ae->wl[ae->idx].l);
		MaxError(ae);
	}
}
void _RRCA(struct s_assenv *ae) {
	if (ae->wl[ae->idx].t) {
		___output(ae,0xF);
	} else {
		rasm_printf(ae,"[%s] Error line %d - RRCA does not need parameter\n",GetCurrentFile(ae),ae->wl[ae->idx].l);
		MaxError(ae);
	}
}
void _NEG(struct s_assenv *ae) {
	if (ae->wl[ae->idx].t) {
		___output(ae,0xED);
		___output(ae,0x44);
	} else {
		rasm_printf(ae,"[%s] Error line %d - NEG does not need parameter\n",GetCurrentFile(ae),ae->wl[ae->idx].l);
		MaxError(ae);
	}
}
void _DAA(struct s_assenv *ae) {
	if (ae->wl[ae->idx].t) {
		___output(ae,0x27);
	} else {
		rasm_printf(ae,"[%s] Error line %d - DAA does not need parameter\n",GetCurrentFile(ae),ae->wl[ae->idx].l);
		MaxError(ae);
	}
}
void _CPL(struct s_assenv *ae) {
	if (ae->wl[ae->idx].t) {
		___output(ae,0x2F);
	} else {
		rasm_printf(ae,"[%s] Error line %d - CPL does not need parameter\n",GetCurrentFile(ae),ae->wl[ae->idx].l);
		MaxError(ae);
	}
}
void _RETI(struct s_assenv *ae) {
	if (ae->wl[ae->idx].t) {
		___output(ae,0xED);
		___output(ae,0x4D);
	} else {
		rasm_printf(ae,"[%s] Error line %d - RETI does not need parameter\n",GetCurrentFile(ae),ae->wl[ae->idx].l);
		MaxError(ae);
	}
}
void _SCF(struct s_assenv *ae) {
	if (ae->wl[ae->idx].t) {
		___output(ae,0x37);
	} else {
		rasm_printf(ae,"[%s] Error line %d - SCF does not need parameter\n",GetCurrentFile(ae),ae->wl[ae->idx].l);
		MaxError(ae);
	}
}
void _LDD(struct s_assenv *ae) {
	if (ae->wl[ae->idx].t) {
		___output(ae,0xED);
		___output(ae,0xA8);
	} else {
		rasm_printf(ae,"[%s] Error line %d - LDD does not need parameter\n",GetCurrentFile(ae),ae->wl[ae->idx].l);
		MaxError(ae);
	}
}
void _LDDR(struct s_assenv *ae) {
	if (ae->wl[ae->idx].t) {
		___output(ae,0xED);
		___output(ae,0xB8);
	} else {
		rasm_printf(ae,"[%s] Error line %d - LDDR does not need parameter\n",GetCurrentFile(ae),ae->wl[ae->idx].l);
		MaxError(ae);
	}
}
void _LDI(struct s_assenv *ae) {
	if (ae->wl[ae->idx].t) {
		___output(ae,0xED);
		___output(ae,0xA0);
	} else {
		rasm_printf(ae,"[%s] Error line %d - LDI does not need parameter\n",GetCurrentFile(ae),ae->wl[ae->idx].l);
		MaxError(ae);
	}
}
void _LDIR(struct s_assenv *ae) {
	if (ae->wl[ae->idx].t) {
		___output(ae,0xED);
		___output(ae,0xB0);
	} else {
		rasm_printf(ae,"[%s] Error line %d - LDIR does not need parameter\n",GetCurrentFile(ae),ae->wl[ae->idx].l);
		MaxError(ae);
	}
}
void _CCF(struct s_assenv *ae) {
	if (ae->wl[ae->idx].t) {
		___output(ae,0x3F);
	} else {
		rasm_printf(ae,"[%s] Error line %d - CCF does not need parameter\n",GetCurrentFile(ae),ae->wl[ae->idx].l);
		MaxError(ae);
	}
}
void _CPD(struct s_assenv *ae) {
	if (ae->wl[ae->idx].t) {
		___output(ae,0xED);
		___output(ae,0xA9);
	} else {
		rasm_printf(ae,"[%s] Error line %d - CPD does not need parameter\n",GetCurrentFile(ae),ae->wl[ae->idx].l);
		MaxError(ae);
	}
}
void _CPDR(struct s_assenv *ae) {
	if (ae->wl[ae->idx].t) {
		___output(ae,0xED);
		___output(ae,0xB9);
	} else {
		rasm_printf(ae,"[%s] Error line %d - CPDR does not need parameter\n",GetCurrentFile(ae),ae->wl[ae->idx].l);
		MaxError(ae);
	}
}
void _CPI(struct s_assenv *ae) {
	if (ae->wl[ae->idx].t) {
		___output(ae,0xED);
		___output(ae,0xA1);
	} else {
		rasm_printf(ae,"[%s] Error line %d - CPI does not need parameter\n",GetCurrentFile(ae),ae->wl[ae->idx].l);
		MaxError(ae);
	}
}
void _CPIR(struct s_assenv *ae) {
	if (ae->wl[ae->idx].t) {
		___output(ae,0xED);
		___output(ae,0xB1);
	} else {
		rasm_printf(ae,"[%s] Error line %d - CPIR does not need parameter\n",GetCurrentFile(ae),ae->wl[ae->idx].l);
		MaxError(ae);
	}
}
void _OUTD(struct s_assenv *ae) {
	if (ae->wl[ae->idx].t) {
		___output(ae,0xED);
		___output(ae,0xAB);
	} else {
		rasm_printf(ae,"[%s] Error line %d - OUTD does not need parameter\n",GetCurrentFile(ae),ae->wl[ae->idx].l);
		MaxError(ae);
	}
}
void _OTDR(struct s_assenv *ae) {
	if (ae->wl[ae->idx].t) {
		___output(ae,0xED);
		___output(ae,0xBB);
	} else {
		rasm_printf(ae,"[%s] Error line %d - OTDR does not need parameter\n",GetCurrentFile(ae),ae->wl[ae->idx].l);
		MaxError(ae);
	}
}
void _OUTI(struct s_assenv *ae) {
	if (ae->wl[ae->idx].t) {
		___output(ae,0xED);
		___output(ae,0xA3);
	} else {
		rasm_printf(ae,"[%s] Error line %d - OUTI does not need parameter\n",GetCurrentFile(ae),ae->wl[ae->idx].l);
		MaxError(ae);
	}
}
void _OTIR(struct s_assenv *ae) {
	if (ae->wl[ae->idx].t) {
		___output(ae,0xED);
		___output(ae,0xB3);
	} else {
		rasm_printf(ae,"[%s] Error line %d - OTIR does not need parameter\n",GetCurrentFile(ae),ae->wl[ae->idx].l);
		MaxError(ae);
	}
}
void _RETN(struct s_assenv *ae) {
	if (ae->wl[ae->idx].t) {
		___output(ae,0xED);
		___output(ae,0x45);
	} else {
		rasm_printf(ae,"[%s] Error line %d - RETN does not need parameter\n",GetCurrentFile(ae),ae->wl[ae->idx].l);
		MaxError(ae);
	}
}
void _IND(struct s_assenv *ae) {
	if (ae->wl[ae->idx].t) {
		___output(ae,0xED);
		___output(ae,0xAA);
	} else {
		rasm_printf(ae,"[%s] Error line %d - IND does not need parameter\n",GetCurrentFile(ae),ae->wl[ae->idx].l);
		MaxError(ae);
	}
}
void _INDR(struct s_assenv *ae) {
	if (ae->wl[ae->idx].t) {
		___output(ae,0xED);
		___output(ae,0xBA);
	} else {
		rasm_printf(ae,"[%s] Error line %d - INDR does not need parameter\n",GetCurrentFile(ae),ae->wl[ae->idx].l);
		MaxError(ae);
	}
}
void _INI(struct s_assenv *ae) {
	if (ae->wl[ae->idx].t) {
		___output(ae,0xED);
		___output(ae,0xA2);
	} else {
		rasm_printf(ae,"[%s] Error line %d - INI does not need parameter\n",GetCurrentFile(ae),ae->wl[ae->idx].l);
		MaxError(ae);
	}
}
void _INIR(struct s_assenv *ae) {
	if (ae->wl[ae->idx].t) {
		___output(ae,0xED);
		___output(ae,0xB2);
	} else {
		rasm_printf(ae,"[%s] Error line %d - INIR does not need parameter\n",GetCurrentFile(ae),ae->wl[ae->idx].l);
		MaxError(ae);
	}
}
void _EXX(struct s_assenv *ae) {
	if (ae->wl[ae->idx].t) {
		___output(ae,0xD9);
	} else {
		rasm_printf(ae,"[%s] Error line %d - EXX does not need parameter\n",GetCurrentFile(ae),ae->wl[ae->idx].l);
		MaxError(ae);
	}
}
void _HALT(struct s_assenv *ae) {
	if (ae->wl[ae->idx].t) {
		___output(ae,0x76);
	} else {
		rasm_printf(ae,"[%s] Error line %d - HALT does not need parameter\n",GetCurrentFile(ae),ae->wl[ae->idx].l);
		MaxError(ae);
	}
}

void _RLA(struct s_assenv *ae) {
	if (ae->wl[ae->idx].t) {
		___output(ae,0x17);
	} else {
		rasm_printf(ae,"[%s] Error line %d - RLA does not need parameter\n",GetCurrentFile(ae),ae->wl[ae->idx].l);
		MaxError(ae);
	}
}
void _RRA(struct s_assenv *ae) {
	if (ae->wl[ae->idx].t) {
		___output(ae,0x1F);
	} else {
		rasm_printf(ae,"[%s] Error line %d - RRA does not need parameter\n",GetCurrentFile(ae),ae->wl[ae->idx].l);
		MaxError(ae);
	}
}
void _RLD(struct s_assenv *ae) {
	if (ae->wl[ae->idx].t) {
		___output(ae,0xED);
		___output(ae,0x6F);
	} else {
		rasm_printf(ae,"[%s] Error line %d - RLD does not need parameter\n",GetCurrentFile(ae),ae->wl[ae->idx].l);
		MaxError(ae);
	}
}
void _RRD(struct s_assenv *ae) {
	if (ae->wl[ae->idx].t) {
		___output(ae,0xED);
		___output(ae,0x67);
	} else {
		rasm_printf(ae,"[%s] Error line %d - RRD does not need parameter\n",GetCurrentFile(ae),ae->wl[ae->idx].l);
		MaxError(ae);
	}
}


void _NOP(struct s_assenv *ae) {
	if (ae->wl[ae->idx].t) {
		___output(ae,0x00);
	} else {
		rasm_printf(ae,"[%s] Error line %d - NOP does not need parameter\n",GetCurrentFile(ae),ae->wl[ae->idx].l);
		MaxError(ae);
	}
}
void _DI(struct s_assenv *ae) {
	if (ae->wl[ae->idx].t) {
	___output(ae,0xF3);
	} else {
		rasm_printf(ae,"[%s] Error line %d - DI does not need parameter\n",GetCurrentFile(ae),ae->wl[ae->idx].l);
		MaxError(ae);
	}
}
void _EI(struct s_assenv *ae) {
	if (ae->wl[ae->idx].t) {
		___output(ae,0xFB);
	} else {
		rasm_printf(ae,"[%s] Error line %d - EI does not need parameter\n",GetCurrentFile(ae),ae->wl[ae->idx].l);
		MaxError(ae);
	}
}

void _RST(struct s_assenv *ae) {
	if (!ae->wl[ae->idx].t && ae->wl[ae->idx+1].t!=2) {
		/* la valeur du paramÃ¨tre va dÃ©finir l'opcode du RST */
		PushExpression(ae,ae->idx+1,E_EXPRESSION_RST);
		ae->idx++;
	} else {
		rasm_printf(ae,"[%s] Error line %d - RST need one parameter\n",GetCurrentFile(ae),ae->wl[ae->idx].l);
		MaxError(ae);
	}
}

void _DJNZ(struct s_assenv *ae) {
	if (!ae->wl[ae->idx].t && ae->wl[ae->idx+1].t!=2) {
		___output(ae,0x10);
		PushExpression(ae,ae->idx+1,E_EXPRESSION_J8);
		ae->idx++;
	} else {
		rasm_printf(ae,"[%s] Error line %d - DJNZ need one parameter\n",GetCurrentFile(ae),ae->wl[ae->idx].l);
		MaxError(ae);
	}
}

void _LD(struct s_assenv *ae) {
	/* on check qu'il y a au moins deux parametres */
	if (!ae->wl[ae->idx+1].t && ae->wl[ae->idx+2].t!=2) {
		switch (GetCRC(ae->wl[ae->idx+1].w)) {
			case CRC_A:
				switch (GetCRC(ae->wl[ae->idx+2].w)) {
					case CRC_I:___output(ae,0xED);___output(ae,0x57);break;
					case CRC_R:___output(ae,0xED);___output(ae,0x5F);break;
					case CRC_B:___output(ae,0x78);break;
					case CRC_C:___output(ae,0x79);break;
					case CRC_D:___output(ae,0x7A);break;
					case CRC_E:___output(ae,0x7B);break;
					case CRC_H:___output(ae,0x7C);break;
					case CRC_L:___output(ae,0x7D);break;
					case CRC_IXH:case CRC_HX:case CRC_XH:___output(ae,0xDD);___output(ae,0x7C);break;
					case CRC_IXL:case CRC_LX:case CRC_XL:___output(ae,0xDD);___output(ae,0x7D);break;
					case CRC_IYH:case CRC_HY:case CRC_YH:___output(ae,0xFD);___output(ae,0x7C);break;
					case CRC_IYL:case CRC_LY:case CRC_YL:___output(ae,0xFD);___output(ae,0x7D);break;
					case CRC_MHL:___output(ae,0x7E);break;
					case CRC_A:___output(ae,0x7F);break;
					case CRC_MBC:___output(ae,0x0A);break;
					case CRC_MDE:___output(ae,0x1A);break;
					default:
					/* (ix+expression) (iy+expression) (expression) expression */
					if (strncmp(ae->wl[ae->idx+2].w,"(IX",3)==0) {
						___output(ae,0xDD);___output(ae,0x7E);
						PushExpression(ae,ae->idx+2,E_EXPRESSION_IV8);
					} else if (strncmp(ae->wl[ae->idx+2].w,"(IY",3)==0) {
						___output(ae,0xFD);___output(ae,0x7E);
						PushExpression(ae,ae->idx+2,E_EXPRESSION_IV8);
					} else if (StringIsMem(ae->wl[ae->idx+2].w)) {
						___output(ae,0x3A);
						PushExpression(ae,ae->idx+2,E_EXPRESSION_V16);
					} else {
						___output(ae,0x3E);
						PushExpression(ae,ae->idx+2,E_EXPRESSION_V8);
					}
				}
				break;
			case CRC_I:
				if (GetCRC(ae->wl[ae->idx+2].w)==CRC_A) {
					___output(ae,0xED);___output(ae,0x47);
				} else {
					rasm_printf(ae,"[%s] Error line %d - LD I,A only\n",GetCurrentFile(ae),ae->wl[ae->idx].l);
					MaxError(ae);
				}
				break;
			case CRC_R:
				if (GetCRC(ae->wl[ae->idx+2].w)==CRC_A) {
					___output(ae,0xED);___output(ae,0x4F);
				} else {
					rasm_printf(ae,"[%s] Error line %d - LD R,A only\n",GetCurrentFile(ae),ae->wl[ae->idx].l);
					MaxError(ae);
				}
				break;
			case CRC_B:
				switch (GetCRC(ae->wl[ae->idx+2].w)) {
					case CRC_B:___output(ae,0x40);break;
					case CRC_C:___output(ae,0x41);break;
					case CRC_D:___output(ae,0x42);break;
					case CRC_E:___output(ae,0x43);break;
					case CRC_H:___output(ae,0x44);break;
					case CRC_L:___output(ae,0x45);break;
					case CRC_IXH:case CRC_HX:case CRC_XH:___output(ae,0xDD);___output(ae,0x44);break;
					case CRC_IXL:case CRC_LX:case CRC_XL:___output(ae,0xDD);___output(ae,0x45);break;
					case CRC_IYH:case CRC_HY:case CRC_YH:___output(ae,0xFD);___output(ae,0x44);break;
					case CRC_IYL:case CRC_LY:case CRC_YL:___output(ae,0xFD);___output(ae,0x45);break;
					case CRC_MHL:___output(ae,0x46);break;
					case CRC_A:___output(ae,0x47);break;
					default:
					/* (ix+expression) (iy+expression) expression */
					if (strncmp(ae->wl[ae->idx+2].w,"(IX",3)==0) {
						___output(ae,0xDD);___output(ae,0x46);
						PushExpression(ae,ae->idx+2,E_EXPRESSION_IV8);
					} else if (strncmp(ae->wl[ae->idx+2].w,"(IY",3)==0) {
						___output(ae,0xFD);___output(ae,0x46);
						PushExpression(ae,ae->idx+2,E_EXPRESSION_IV8);
					} else {
						___output(ae,0x06);
						PushExpression(ae,ae->idx+2,E_EXPRESSION_V8);
					}
				}
				break;
			case CRC_C:
				switch (GetCRC(ae->wl[ae->idx+2].w)) {
					case CRC_B:___output(ae,0x48);break;
					case CRC_C:___output(ae,0x49);break;
					case CRC_D:___output(ae,0x4A);break;
					case CRC_E:___output(ae,0x4B);break;
					case CRC_H:___output(ae,0x4C);break;
					case CRC_L:___output(ae,0x4D);break;
					case CRC_IXH:case CRC_HX:case CRC_XH:___output(ae,0xDD);___output(ae,0x4C);break;
					case CRC_IXL:case CRC_LX:case CRC_XL:___output(ae,0xDD);___output(ae,0x4D);break;
					case CRC_IYH:case CRC_HY:case CRC_YH:___output(ae,0xFD);___output(ae,0x4C);break;
					case CRC_IYL:case CRC_LY:case CRC_YL:___output(ae,0xFD);___output(ae,0x4D);break;
					case CRC_MHL:___output(ae,0x4E);break;
					case CRC_A:___output(ae,0x4F);break;
					default:
					/* (ix+expression) (iy+expression) expression */
					if (strncmp(ae->wl[ae->idx+2].w,"(IX",3)==0) {
						___output(ae,0xDD);___output(ae,0x4E);
						PushExpression(ae,ae->idx+2,E_EXPRESSION_IV8);
					} else if (strncmp(ae->wl[ae->idx+2].w,"(IY",3)==0) {
						___output(ae,0xFD);___output(ae,0x4E);
						PushExpression(ae,ae->idx+2,E_EXPRESSION_IV8);
					} else {
						___output(ae,0x0E);
						PushExpression(ae,ae->idx+2,E_EXPRESSION_V8);
					}
				}
				break;
			case CRC_D:
				switch (GetCRC(ae->wl[ae->idx+2].w)) {
					case CRC_B:___output(ae,0x50);break;
					case CRC_C:___output(ae,0x51);break;
					case CRC_D:___output(ae,0x52);break;
					case CRC_E:___output(ae,0x53);break;
					case CRC_H:___output(ae,0x54);break;
					case CRC_L:___output(ae,0x55);break;
					case CRC_IXH:case CRC_HX:case CRC_XH:___output(ae,0xDD);___output(ae,0x54);break;
					case CRC_IXL:case CRC_LX:case CRC_XL:___output(ae,0xDD);___output(ae,0x55);break;
					case CRC_IYH:case CRC_HY:case CRC_YH:___output(ae,0xFD);___output(ae,0x54);break;
					case CRC_IYL:case CRC_LY:case CRC_YL:___output(ae,0xFD);___output(ae,0x55);break;
					case CRC_MHL:___output(ae,0x56);break;
					case CRC_A:___output(ae,0x57);break;
					default:
					/* (ix+expression) (iy+expression) expression */
					if (strncmp(ae->wl[ae->idx+2].w,"(IX",3)==0) {
						___output(ae,0xDD);___output(ae,0x56);
						PushExpression(ae,ae->idx+2,E_EXPRESSION_IV8);
					} else if (strncmp(ae->wl[ae->idx+2].w,"(IY",3)==0) {
						___output(ae,0xFD);___output(ae,0x56);
						PushExpression(ae,ae->idx+2,E_EXPRESSION_IV8);
					} else {
						___output(ae,0x16);
						PushExpression(ae,ae->idx+2,E_EXPRESSION_V8);
					}
				}
				break;
			case CRC_E:
				switch (GetCRC(ae->wl[ae->idx+2].w)) {
					case CRC_B:___output(ae,0x58);break;
					case CRC_C:___output(ae,0x59);break;
					case CRC_D:___output(ae,0x5A);break;
					case CRC_E:___output(ae,0x5B);break;
					case CRC_H:___output(ae,0x5C);break;
					case CRC_L:___output(ae,0x5D);break;
					case CRC_IXH:case CRC_HX:case CRC_XH:___output(ae,0xDD);___output(ae,0x5C);break;
					case CRC_IXL:case CRC_LX:case CRC_XL:___output(ae,0xDD);___output(ae,0x5D);break;
					case CRC_IYH:case CRC_HY:case CRC_YH:___output(ae,0xFD);___output(ae,0x5C);break;
					case CRC_IYL:case CRC_LY:case CRC_YL:___output(ae,0xFD);___output(ae,0x5D);break;
					case CRC_MHL:___output(ae,0x5E);break;
					case CRC_A:___output(ae,0x5F);break;
					default:
					/* (ix+expression) (iy+expression) expression */
					if (strncmp(ae->wl[ae->idx+2].w,"(IX",3)==0) {
						___output(ae,0xDD);___output(ae,0x5E);
						PushExpression(ae,ae->idx+2,E_EXPRESSION_IV8);
					} else if (strncmp(ae->wl[ae->idx+2].w,"(IY",3)==0) {
						___output(ae,0xFD);___output(ae,0x5E);
						PushExpression(ae,ae->idx+2,E_EXPRESSION_IV8);
					} else {
						___output(ae,0x1E);
						PushExpression(ae,ae->idx+2,E_EXPRESSION_V8);
					}
				}
				break;
			case CRC_IYH:case CRC_HY:case CRC_YH:
				switch (GetCRC(ae->wl[ae->idx+2].w)) {
					case CRC_B:___output(ae,0xFD);___output(ae,0x60);break;
					case CRC_C:___output(ae,0xFD);___output(ae,0x61);break;
					case CRC_D:___output(ae,0xFD);___output(ae,0x62);break;
					case CRC_E:___output(ae,0xFD);___output(ae,0x63);break;
					case CRC_IYH:case CRC_HY:case CRC_YH:___output(ae,0xFD);___output(ae,0x64);break;
					case CRC_IYL:case CRC_LY:case CRC_YL:___output(ae,0xFD);___output(ae,0x65);break;
					case CRC_A:___output(ae,0xFD);___output(ae,0x67);break;
					default:
						___output(ae,0xFD);___output(ae,0x26);
						PushExpression(ae,ae->idx+2,E_EXPRESSION_V8);
				}
				break;
			case CRC_IYL:case CRC_LY:case CRC_YL:
				switch (GetCRC(ae->wl[ae->idx+2].w)) {
					case CRC_B:___output(ae,0xFD);___output(ae,0x68);break;
					case CRC_C:___output(ae,0xFD);___output(ae,0x69);break;
					case CRC_D:___output(ae,0xFD);___output(ae,0x6A);break;
					case CRC_E:___output(ae,0xFD);___output(ae,0x6B);break;
					case CRC_IYH:case CRC_HY:case CRC_YH:___output(ae,0xFD);___output(ae,0x6C);break;
					case CRC_IYL:case CRC_LY:case CRC_YL:___output(ae,0xFD);___output(ae,0x6D);break;
					case CRC_A:___output(ae,0xFD);___output(ae,0x6F);break;
					default:
						___output(ae,0xFD);___output(ae,0x2E);
						PushExpression(ae,ae->idx+2,E_EXPRESSION_V8);
				}
				break;
			case CRC_IXH:case CRC_HX:case CRC_XH:
				switch (GetCRC(ae->wl[ae->idx+2].w)) {
					case CRC_B:___output(ae,0xDD);___output(ae,0x60);break;
					case CRC_C:___output(ae,0xDD);___output(ae,0x61);break;
					case CRC_D:___output(ae,0xDD);___output(ae,0x62);break;
					case CRC_E:___output(ae,0xDD);___output(ae,0x63);break;
					case CRC_IXH:case CRC_HX:case CRC_XH:___output(ae,0xDD);___output(ae,0x64);break;
					case CRC_IXL:case CRC_LX:case CRC_XL:___output(ae,0xDD);___output(ae,0x65);break;
					case CRC_A:___output(ae,0xDD);___output(ae,0x67);break;
					default:
						___output(ae,0xDD);___output(ae,0x26);
						PushExpression(ae,ae->idx+2,E_EXPRESSION_V8);
				}
				break;
			case CRC_IXL:case CRC_LX:case CRC_XL:
				switch (GetCRC(ae->wl[ae->idx+2].w)) {
					case CRC_B:___output(ae,0xDD);___output(ae,0x68);break;
					case CRC_C:___output(ae,0xDD);___output(ae,0x69);break;
					case CRC_D:___output(ae,0xDD);___output(ae,0x6A);break;
					case CRC_E:___output(ae,0xDD);___output(ae,0x6B);break;
					case CRC_IXH:case CRC_HX:case CRC_XH:___output(ae,0xDD);___output(ae,0x6C);break;
					case CRC_IXL:case CRC_LX:case CRC_XL:___output(ae,0xDD);___output(ae,0x6D);break;
					case CRC_A:___output(ae,0xDD);___output(ae,0x6F);break;
					default:
						___output(ae,0xDD);___output(ae,0x2E);
						PushExpression(ae,ae->idx+2,E_EXPRESSION_V8);
				}
				break;
			case CRC_H:
				switch (GetCRC(ae->wl[ae->idx+2].w)) {
					case CRC_B:___output(ae,0x60);break;
					case CRC_C:___output(ae,0x61);break;
					case CRC_D:___output(ae,0x62);break;
					case CRC_E:___output(ae,0x63);break;
					case CRC_H:___output(ae,0x64);break;
					case CRC_L:___output(ae,0x65);break;
					case CRC_MHL:___output(ae,0x66);break;
					case CRC_A:___output(ae,0x67);break;
					default:
					/* (ix+expression) (iy+expression) expression */
					if (strncmp(ae->wl[ae->idx+2].w,"(IX",3)==0) {
						___output(ae,0xDD);___output(ae,0x66);
						PushExpression(ae,ae->idx+2,E_EXPRESSION_IV8);
					} else if (strncmp(ae->wl[ae->idx+2].w,"(IY",3)==0) {
						___output(ae,0xFD);___output(ae,0x66);
						PushExpression(ae,ae->idx+2,E_EXPRESSION_IV8);
					} else {
						___output(ae,0x26);
						PushExpression(ae,ae->idx+2,E_EXPRESSION_V8);
					}
				}
				break;
			case CRC_L:
				switch (GetCRC(ae->wl[ae->idx+2].w)) {
					case CRC_B:___output(ae,0x68);break;
					case CRC_C:___output(ae,0x69);break;
					case CRC_D:___output(ae,0x6A);break;
					case CRC_E:___output(ae,0x6B);break;
					case CRC_H:___output(ae,0x6C);break;
					case CRC_L:___output(ae,0x6D);break;
					case CRC_MHL:___output(ae,0x6E);break;
					case CRC_A:___output(ae,0x6F);break;
					default:
					/* (ix+expression) (iy+expression) expression */
					if (strncmp(ae->wl[ae->idx+2].w,"(IX",3)==0) {
						___output(ae,0xDD);___output(ae,0x6E);
						PushExpression(ae,ae->idx+2,E_EXPRESSION_IV8);
					} else if (strncmp(ae->wl[ae->idx+2].w,"(IY",3)==0) {
						___output(ae,0xFD);___output(ae,0x6E);
						PushExpression(ae,ae->idx+2,E_EXPRESSION_IV8);
					} else {
						___output(ae,0x2E);
						PushExpression(ae,ae->idx+2,E_EXPRESSION_V8);
					}
				}
				break;
			case CRC_MHL:
				switch (GetCRC(ae->wl[ae->idx+2].w)) {
					case CRC_B:___output(ae,0x70);break;
					case CRC_C:___output(ae,0x71);break;
					case CRC_D:___output(ae,0x72);break;
					case CRC_E:___output(ae,0x73);break;
					case CRC_H:___output(ae,0x74);break;
					case CRC_L:___output(ae,0x75);break;
					case CRC_A:___output(ae,0x77);break;
					default:
					/* expression */
					___output(ae,0x36);
					PushExpression(ae,ae->idx+2,E_EXPRESSION_V8);
				}
				break;
			case CRC_MBC:
				if (GetCRC(ae->wl[ae->idx+2].w)==CRC_A)  {
					___output(ae,0x02);
				} else {
					rasm_printf(ae,"[%s] Error line %d - LD (BC),A only\n",GetCurrentFile(ae),ae->wl[ae->idx].l);
					MaxError(ae);
				}
				break;
			case CRC_MDE:
				if (GetCRC(ae->wl[ae->idx+2].w)==CRC_A)  {
					___output(ae,0x12);
				} else {
					rasm_printf(ae,"[%s] Error line %d - LD (DE),A only\n",GetCurrentFile(ae),ae->wl[ae->idx].l);
					MaxError(ae);
				}
				break;
			case CRC_HL:
				if (StringIsMem(ae->wl[ae->idx+2].w)) {
					___output(ae,0x2A);
					PushExpression(ae,ae->idx+2,E_EXPRESSION_V16);
				} else {
					___output(ae,0x21);
					PushExpression(ae,ae->idx+2,E_EXPRESSION_V16);
				}
				break;
			case CRC_BC:
				if (StringIsMem(ae->wl[ae->idx+2].w)) {
					___output(ae,0xED);___output(ae,0x4B);
					PushExpression(ae,ae->idx+2,E_EXPRESSION_IV16);
				} else {
					___output(ae,0x01);
					PushExpression(ae,ae->idx+2,E_EXPRESSION_V16);
				}
				break;
			case CRC_DE:
				if (StringIsMem(ae->wl[ae->idx+2].w)) {
					___output(ae,0xED);___output(ae,0x5B);
					PushExpression(ae,ae->idx+2,E_EXPRESSION_IV16);
				} else {
					___output(ae,0x11);
					PushExpression(ae,ae->idx+2,E_EXPRESSION_V16);
				}
				break;
			case CRC_IX:
				if (StringIsMem(ae->wl[ae->idx+2].w)) {
					___output(ae,0xDD);___output(ae,0x2A);
					PushExpression(ae,ae->idx+2,E_EXPRESSION_IV16);
				} else {
					___output(ae,0xDD);___output(ae,0x21);
					PushExpression(ae,ae->idx+2,E_EXPRESSION_IV16);
				}
				break;
			case CRC_IY:
				if (StringIsMem(ae->wl[ae->idx+2].w)) {
					___output(ae,0xFD);___output(ae,0x2A);
					PushExpression(ae,ae->idx+2,E_EXPRESSION_IV16);
				} else {
					___output(ae,0xFD);___output(ae,0x21);
					PushExpression(ae,ae->idx+2,E_EXPRESSION_IV16);
				}
				break;
			case CRC_SP:
				switch (GetCRC(ae->wl[ae->idx+2].w)) {
					case CRC_HL:___output(ae,0xF9);break;
					case CRC_IX:___output(ae,0xDD);___output(ae,0xF9);break;
					case CRC_IY:___output(ae,0xFD);___output(ae,0xF9);break;
					default:
						if (StringIsMem(ae->wl[ae->idx+2].w)) {
							___output(ae,0xED);___output(ae,0x7B);
							PushExpression(ae,ae->idx+2,E_EXPRESSION_IV16);
						} else {
							___output(ae,0x31);
							PushExpression(ae,ae->idx+2,E_EXPRESSION_V16);
						}
				}
				break;
			default:
				/* (ix+expression) (iy+expression) (expression) expression */
				if (strncmp(ae->wl[ae->idx+1].w,"(IX",3)==0) {
					switch (GetCRC(ae->wl[ae->idx+2].w)) {
						case CRC_B:___output(ae,0xDD);___output(ae,0x70);PushExpression(ae,ae->idx+1,E_EXPRESSION_IV8);break;
						case CRC_C:___output(ae,0xDD);___output(ae,0x71);PushExpression(ae,ae->idx+1,E_EXPRESSION_IV8);break;
						case CRC_D:___output(ae,0xDD);___output(ae,0x72);PushExpression(ae,ae->idx+1,E_EXPRESSION_IV8);break;
						case CRC_E:___output(ae,0xDD);___output(ae,0x73);PushExpression(ae,ae->idx+1,E_EXPRESSION_IV8);break;
						case CRC_H:___output(ae,0xDD);___output(ae,0x74);PushExpression(ae,ae->idx+1,E_EXPRESSION_IV8);break;
						case CRC_L:___output(ae,0xDD);___output(ae,0x75);PushExpression(ae,ae->idx+1,E_EXPRESSION_IV8);break;
						case CRC_A:___output(ae,0xDD);___output(ae,0x77);PushExpression(ae,ae->idx+1,E_EXPRESSION_IV8);break;
						default:___output(ae,0xDD);___output(ae,0x36);
							PushExpression(ae,ae->idx+1,E_EXPRESSION_IV8);
							PushExpression(ae,ae->idx+2,E_EXPRESSION_3V8);
					}
				} else if (strncmp(ae->wl[ae->idx+1].w,"(IY",3)==0) {
					switch (GetCRC(ae->wl[ae->idx+2].w)) {
						case CRC_B:___output(ae,0xFD);___output(ae,0x70);PushExpression(ae,ae->idx+1,E_EXPRESSION_IV8);break;
						case CRC_C:___output(ae,0xFD);___output(ae,0x71);PushExpression(ae,ae->idx+1,E_EXPRESSION_IV8);break;
						case CRC_D:___output(ae,0xFD);___output(ae,0x72);PushExpression(ae,ae->idx+1,E_EXPRESSION_IV8);break;
						case CRC_E:___output(ae,0xFD);___output(ae,0x73);PushExpression(ae,ae->idx+1,E_EXPRESSION_IV8);break;
						case CRC_H:___output(ae,0xFD);___output(ae,0x74);PushExpression(ae,ae->idx+1,E_EXPRESSION_IV8);break;
						case CRC_L:___output(ae,0xFD);___output(ae,0x75);PushExpression(ae,ae->idx+1,E_EXPRESSION_IV8);break;
						case CRC_A:___output(ae,0xFD);___output(ae,0x77);PushExpression(ae,ae->idx+1,E_EXPRESSION_IV8);break;
						default:___output(ae,0xFD);___output(ae,0x36);
							PushExpression(ae,ae->idx+1,E_EXPRESSION_IV8);
							PushExpression(ae,ae->idx+2,E_EXPRESSION_3V8);
					}
				} else if (StringIsMem(ae->wl[ae->idx+1].w)) {
					switch (GetCRC(ae->wl[ae->idx+2].w)) {
						case CRC_A:___output(ae,0x32);PushExpression(ae,ae->idx+1,E_EXPRESSION_V16);break;
						case CRC_BC:___output(ae,0xED);___output(ae,0x43);PushExpression(ae,ae->idx+1,E_EXPRESSION_IV16);break;
						case CRC_DE:___output(ae,0xED);___output(ae,0x53);PushExpression(ae,ae->idx+1,E_EXPRESSION_IV16);break;
						case CRC_HL:___output(ae,0x22);PushExpression(ae,ae->idx+1,E_EXPRESSION_V16);break;
						case CRC_IX:___output(ae,0xDD);___output(ae,0x22);PushExpression(ae,ae->idx+1,E_EXPRESSION_IV16);break;
						case CRC_IY:___output(ae,0xFD);___output(ae,0x22);PushExpression(ae,ae->idx+1,E_EXPRESSION_IV16);break;
						case CRC_SP:___output(ae,0xED);___output(ae,0x73);PushExpression(ae,ae->idx+1,E_EXPRESSION_IV16);break;
						default:
							rasm_printf(ae,"[%s] Error line %d - LD (#nnnn),[A,BC,DE,HL,SP,IX,IY] only\n",GetCurrentFile(ae),ae->wl[ae->idx].l);
							MaxError(ae);
					}
				} else {
					rasm_printf(ae,"[%s] Error line %d - Unknown LD format\n",GetCurrentFile(ae),ae->wl[ae->idx].l);
					MaxError(ae);
				}
				break;
		}
		ae->idx+=2;
	} else {
		rasm_printf(ae,"[%s] Error line %d - LD needs two parameters\n",GetCurrentFile(ae),ae->wl[ae->idx].l);
		MaxError(ae);
	}
}


void _RLC(struct s_assenv *ae) {
	/* on check qu'il y a un ou deux paramÃ¨tres */
	if (ae->wl[ae->idx+1].t==1) {
		switch (GetCRC(ae->wl[ae->idx+1].w)) {
			case CRC_B:___output(ae,0xCB);___output(ae,0x0);break;
			case CRC_C:___output(ae,0xCB);___output(ae,0x1);break;
			case CRC_D:___output(ae,0xCB);___output(ae,0x2);break;
			case CRC_E:___output(ae,0xCB);___output(ae,0x3);break;
			case CRC_H:___output(ae,0xCB);___output(ae,0x4);break;
			case CRC_L:___output(ae,0xCB);___output(ae,0x5);break;
			case CRC_MHL:___output(ae,0xCB);___output(ae,0x6);break;
			case CRC_A:___output(ae,0xCB);___output(ae,0x7);break;
			default:
				if (strncmp(ae->wl[ae->idx+1].w,"(IX",3)==0) {
					___output(ae,0xDD);___output(ae,0xCB);
					PushExpression(ae,ae->idx+1,E_EXPRESSION_IV8);
					___output(ae,0x6);
				} else if (strncmp(ae->wl[ae->idx+1].w,"(IY",3)==0) {
					___output(ae,0xFD);___output(ae,0xCB);
					PushExpression(ae,ae->idx+1,E_EXPRESSION_IV8);
					___output(ae,0x6);
				} else {
					rasm_printf(ae,"[%s] Error line %d - syntax is RLC reg8/(HL)/(IX+n)/(IY+n)\n",GetCurrentFile(ae),ae->wl[ae->idx].l);
					MaxError(ae);
				}
		}
		ae->idx++;
	} else if (!ae->wl[ae->idx+1].t && ae->wl[ae->idx+2].t!=2) {
		if (strncmp(ae->wl[ae->idx+1].w,"(IX",3)==0) {
			___output(ae,0xDD);
		} else if (strncmp(ae->wl[ae->idx+1].w,"(IY",3)==0) {
			___output(ae,0xFD);
		} else {
			rasm_printf(ae,"(%s] Error line %d - syntax is RLC (IX+n),reg8\n",GetCurrentFile(ae),ae->wl[ae->idx].l);
			MaxError(ae);
		}
		___output(ae,0xCB);
		switch (GetCRC(ae->wl[ae->idx+2].w)) {
			case CRC_B:PushExpression(ae,ae->idx+1,E_EXPRESSION_IV8);___output(ae,0x0);break;
			case CRC_C:PushExpression(ae,ae->idx+1,E_EXPRESSION_IV8);___output(ae,0x1);break;
			case CRC_D:PushExpression(ae,ae->idx+1,E_EXPRESSION_IV8);___output(ae,0x2);break;
			case CRC_E:PushExpression(ae,ae->idx+1,E_EXPRESSION_IV8);___output(ae,0x3);break;
			case CRC_H:PushExpression(ae,ae->idx+1,E_EXPRESSION_IV8);___output(ae,0x4);break;
			case CRC_L:PushExpression(ae,ae->idx+1,E_EXPRESSION_IV8);___output(ae,0x5);break;
			case CRC_A:PushExpression(ae,ae->idx+1,E_EXPRESSION_IV8);___output(ae,0x7);break;
			default:			
				rasm_printf(ae,"[%s] Error line %d - syntax is RLC (IX+n),reg8\n",GetCurrentFile(ae),ae->wl[ae->idx].l);
				MaxError(ae);
		}
		ae->idx++;
		ae->idx++;
	}
}

void _RRC(struct s_assenv *ae) {
	/* on check qu'il y a un ou deux paramÃ¨tres */
	if (ae->wl[ae->idx+1].t==1) {
		switch (GetCRC(ae->wl[ae->idx+1].w)) {
			case CRC_B:___output(ae,0xCB);___output(ae,0x8);break;
			case CRC_C:___output(ae,0xCB);___output(ae,0x9);break;
			case CRC_D:___output(ae,0xCB);___output(ae,0xA);break;
			case CRC_E:___output(ae,0xCB);___output(ae,0xB);break;
			case CRC_H:___output(ae,0xCB);___output(ae,0xC);break;
			case CRC_L:___output(ae,0xCB);___output(ae,0xD);break;
			case CRC_MHL:___output(ae,0xCB);___output(ae,0xE);break;
			case CRC_A:___output(ae,0xCB);___output(ae,0xF);break;
			default:
				if (strncmp(ae->wl[ae->idx+1].w,"(IX",3)==0) {
					___output(ae,0xDD);___output(ae,0xCB);
					PushExpression(ae,ae->idx+1,E_EXPRESSION_IV8);
					___output(ae,0xE);
				} else if (strncmp(ae->wl[ae->idx+1].w,"(IY",3)==0) {
					___output(ae,0xFD);___output(ae,0xCB);
					PushExpression(ae,ae->idx+1,E_EXPRESSION_IV8);
					___output(ae,0xE);
				} else {
					rasm_printf(ae,"[%s] Error line %d - syntax is RRC reg8/(HL)/(IX+n)/(IY+n)\n",GetCurrentFile(ae),ae->wl[ae->idx].l);
					MaxError(ae);
				}
		}
		ae->idx++;
	} else if (!ae->wl[ae->idx+1].t && ae->wl[ae->idx+2].t!=2) {
		if (strncmp(ae->wl[ae->idx+1].w,"(IX",3)==0) {
			___output(ae,0xDD);
		} else if (strncmp(ae->wl[ae->idx+1].w,"(IY",3)==0) {
			___output(ae,0xFD);
		} else {
			rasm_printf(ae,"[%s] Error line %d - syntax is RRC (IX+n),reg8\n",GetCurrentFile(ae),ae->wl[ae->idx].l);
			MaxError(ae);
		}
		___output(ae,0xCB);
		switch (GetCRC(ae->wl[ae->idx+2].w)) {
			case CRC_B:PushExpression(ae,ae->idx+1,E_EXPRESSION_IV8);___output(ae,0x8);break;
			case CRC_C:PushExpression(ae,ae->idx+1,E_EXPRESSION_IV8);___output(ae,0x9);break;
			case CRC_D:PushExpression(ae,ae->idx+1,E_EXPRESSION_IV8);___output(ae,0xA);break;
			case CRC_E:PushExpression(ae,ae->idx+1,E_EXPRESSION_IV8);___output(ae,0xB);break;
			case CRC_H:PushExpression(ae,ae->idx+1,E_EXPRESSION_IV8);___output(ae,0xC);break;
			case CRC_L:PushExpression(ae,ae->idx+1,E_EXPRESSION_IV8);___output(ae,0xD);break;
			case CRC_A:PushExpression(ae,ae->idx+1,E_EXPRESSION_IV8);___output(ae,0xF);break;
			default:			
				rasm_printf(ae,"[%s] Error line %d - syntax is RRC (IX+n),reg8\n",GetCurrentFile(ae),ae->wl[ae->idx].l);
				MaxError(ae);
		}
		ae->idx++;
		ae->idx++;
	}
}


void _RL(struct s_assenv *ae) {
	/* on check qu'il y a un ou deux paramÃ¨tres */
	if (ae->wl[ae->idx+1].t==1) {
		switch (GetCRC(ae->wl[ae->idx+1].w)) {
			case CRC_B:___output(ae,0xCB);___output(ae,0x10);break;
			case CRC_C:___output(ae,0xCB);___output(ae,0x11);break;
			case CRC_D:___output(ae,0xCB);___output(ae,0x12);break;
			case CRC_E:___output(ae,0xCB);___output(ae,0x13);break;
			case CRC_H:___output(ae,0xCB);___output(ae,0x14);break;
			case CRC_L:___output(ae,0xCB);___output(ae,0x15);break;
			case CRC_MHL:___output(ae,0xCB);___output(ae,0x16);break;
			case CRC_A:___output(ae,0xCB);___output(ae,0x17);break;
			default:
				if (strncmp(ae->wl[ae->idx+1].w,"(IX",3)==0) {
					___output(ae,0xDD);___output(ae,0xCB);
					PushExpression(ae,ae->idx+1,E_EXPRESSION_IV8);
					___output(ae,0x16);
				} else if (strncmp(ae->wl[ae->idx+1].w,"(IY",3)==0) {
					___output(ae,0xFD);___output(ae,0xCB);
					PushExpression(ae,ae->idx+1,E_EXPRESSION_IV8);
					___output(ae,0x16);
				} else {
					rasm_printf(ae,"[%s] Error line %d - syntax is RL reg8/(HL)/(IX+n)/(IY+n)\n",GetCurrentFile(ae),ae->wl[ae->idx].l);
					MaxError(ae);
				}
		}
		ae->idx++;
	} else if (!ae->wl[ae->idx+1].t && ae->wl[ae->idx+2].t!=2) {
		if (strncmp(ae->wl[ae->idx+1].w,"(IX",3)==0) {
			___output(ae,0xDD);
		} else if (strncmp(ae->wl[ae->idx+1].w,"(IY",3)==0) {
			___output(ae,0xFD);
		} else {
			rasm_printf(ae,"[%s] Error line %d - syntax is RL (IX+n),reg8\n",GetCurrentFile(ae),ae->wl[ae->idx].l);
			MaxError(ae);
		}
		___output(ae,0xCB);
		switch (GetCRC(ae->wl[ae->idx+2].w)) {
			case CRC_B:PushExpression(ae,ae->idx+1,E_EXPRESSION_IV8);___output(ae,0x10);break;
			case CRC_C:PushExpression(ae,ae->idx+1,E_EXPRESSION_IV8);___output(ae,0x11);break;
			case CRC_D:PushExpression(ae,ae->idx+1,E_EXPRESSION_IV8);___output(ae,0x12);break;
			case CRC_E:PushExpression(ae,ae->idx+1,E_EXPRESSION_IV8);___output(ae,0x13);break;
			case CRC_H:PushExpression(ae,ae->idx+1,E_EXPRESSION_IV8);___output(ae,0x14);break;
			case CRC_L:PushExpression(ae,ae->idx+1,E_EXPRESSION_IV8);___output(ae,0x15);break;
			case CRC_A:PushExpression(ae,ae->idx+1,E_EXPRESSION_IV8);___output(ae,0x17);break;
			default:			
				rasm_printf(ae,"[%s] Error line %d - syntax is RL (IX+n),reg8\n",GetCurrentFile(ae),ae->wl[ae->idx].l);
				MaxError(ae);
		}
		ae->idx++;
		ae->idx++;
	}
}

void _RR(struct s_assenv *ae) {
	/* on check qu'il y a un ou deux paramÃ¨tres */
	if (ae->wl[ae->idx+1].t==1) {
		switch (GetCRC(ae->wl[ae->idx+1].w)) {
			case CRC_B:___output(ae,0xCB);___output(ae,0x18);break;
			case CRC_C:___output(ae,0xCB);___output(ae,0x19);break;
			case CRC_D:___output(ae,0xCB);___output(ae,0x1A);break;
			case CRC_E:___output(ae,0xCB);___output(ae,0x1B);break;
			case CRC_H:___output(ae,0xCB);___output(ae,0x1C);break;
			case CRC_L:___output(ae,0xCB);___output(ae,0x1D);break;
			case CRC_MHL:___output(ae,0xCB);___output(ae,0x1E);break;
			case CRC_A:___output(ae,0xCB);___output(ae,0x1F);break;
			default:
				if (strncmp(ae->wl[ae->idx+1].w,"(IX",3)==0) {
					___output(ae,0xDD);___output(ae,0xCB);
					PushExpression(ae,ae->idx+1,E_EXPRESSION_IV8);
					___output(ae,0x1E);
				} else if (strncmp(ae->wl[ae->idx+1].w,"(IY",3)==0) {
					___output(ae,0xFD);___output(ae,0xCB);
					PushExpression(ae,ae->idx+1,E_EXPRESSION_IV8);
					___output(ae,0x1E);
				} else {
					rasm_printf(ae,"[%s] Error line %d - syntax is RR reg8/(HL)/(IX+n)/(IY+n)\n",GetCurrentFile(ae),ae->wl[ae->idx].l);
					MaxError(ae);
				}
		}
		ae->idx++;
	} else if (!ae->wl[ae->idx+1].t && ae->wl[ae->idx+2].t!=2) {
		if (strncmp(ae->wl[ae->idx+1].w,"(IX",3)==0) {
			___output(ae,0xDD);
		} else if (strncmp(ae->wl[ae->idx+1].w,"(IY",3)==0) {
			___output(ae,0xFD);
		} else {
			rasm_printf(ae,"[%s] Error line %d - syntax is RR (IX+n),reg8\n",GetCurrentFile(ae),ae->wl[ae->idx].l);
			MaxError(ae);
		}
		___output(ae,0xCB);
		switch (GetCRC(ae->wl[ae->idx+2].w)) {
			case CRC_B:PushExpression(ae,ae->idx+1,E_EXPRESSION_IV8);___output(ae,0x18);break;
			case CRC_C:PushExpression(ae,ae->idx+1,E_EXPRESSION_IV8);___output(ae,0x19);break;
			case CRC_D:PushExpression(ae,ae->idx+1,E_EXPRESSION_IV8);___output(ae,0x1A);break;
			case CRC_E:PushExpression(ae,ae->idx+1,E_EXPRESSION_IV8);___output(ae,0x1B);break;
			case CRC_H:PushExpression(ae,ae->idx+1,E_EXPRESSION_IV8);___output(ae,0x1C);break;
			case CRC_L:PushExpression(ae,ae->idx+1,E_EXPRESSION_IV8);___output(ae,0x1D);break;
			case CRC_A:PushExpression(ae,ae->idx+1,E_EXPRESSION_IV8);___output(ae,0x1F);break;
			default:			
				rasm_printf(ae,"[%s] Error line %d - syntax is RR (IX+n),reg8\n",GetCurrentFile(ae),ae->wl[ae->idx].l);
				MaxError(ae);
		}
		ae->idx++;
		ae->idx++;
	}
}





void _SLA(struct s_assenv *ae) {
	/* on check qu'il y a un ou deux paramÃ¨tres */
	if (ae->wl[ae->idx+1].t==1) {
		switch (GetCRC(ae->wl[ae->idx+1].w)) {
			case CRC_B:___output(ae,0xCB);___output(ae,0x20);break;
			case CRC_C:___output(ae,0xCB);___output(ae,0x21);break;
			case CRC_D:___output(ae,0xCB);___output(ae,0x22);break;
			case CRC_E:___output(ae,0xCB);___output(ae,0x23);break;
			case CRC_H:___output(ae,0xCB);___output(ae,0x24);break;
			case CRC_L:___output(ae,0xCB);___output(ae,0x25);break;
			case CRC_MHL:___output(ae,0xCB);___output(ae,0x26);break;
			case CRC_A:___output(ae,0xCB);___output(ae,0x27);break;
			default:
				if (strncmp(ae->wl[ae->idx+1].w,"(IX",3)==0) {
					___output(ae,0xDD);___output(ae,0xCB);
					PushExpression(ae,ae->idx+1,E_EXPRESSION_IV8);
					___output(ae,0x26);
				} else if (strncmp(ae->wl[ae->idx+1].w,"(IY",3)==0) {
					___output(ae,0xFD);___output(ae,0xCB);
					PushExpression(ae,ae->idx+1,E_EXPRESSION_IV8);
					___output(ae,0x26);
				} else {
					rasm_printf(ae,"[%s] Error line %d - syntax is SLA reg8/(HL)/(IX+n)/(IY+n)\n",GetCurrentFile(ae),ae->wl[ae->idx].l);
					MaxError(ae);
				}
		}
		ae->idx++;
	} else if (!ae->wl[ae->idx+1].t && ae->wl[ae->idx+2].t!=2) {
		if (strncmp(ae->wl[ae->idx+1].w,"(IX",3)==0) {
			___output(ae,0xDD);
		} else if (strncmp(ae->wl[ae->idx+1].w,"(IY",3)==0) {
			___output(ae,0xFD);
		} else {
			rasm_printf(ae,"[%s] Error line %d - syntax is SLL (IX+n),reg8\n",GetCurrentFile(ae),ae->wl[ae->idx].l);
			MaxError(ae);
		}
		___output(ae,0xCB);
		switch (GetCRC(ae->wl[ae->idx+2].w)) {
			case CRC_B:PushExpression(ae,ae->idx+1,E_EXPRESSION_IV8);___output(ae,0x20);break;
			case CRC_C:PushExpression(ae,ae->idx+1,E_EXPRESSION_IV8);___output(ae,0x21);break;
			case CRC_D:PushExpression(ae,ae->idx+1,E_EXPRESSION_IV8);___output(ae,0x22);break;
			case CRC_E:PushExpression(ae,ae->idx+1,E_EXPRESSION_IV8);___output(ae,0x23);break;
			case CRC_H:PushExpression(ae,ae->idx+1,E_EXPRESSION_IV8);___output(ae,0x24);break;
			case CRC_L:PushExpression(ae,ae->idx+1,E_EXPRESSION_IV8);___output(ae,0x25);break;
			case CRC_A:PushExpression(ae,ae->idx+1,E_EXPRESSION_IV8);___output(ae,0x27);break;
			default:			
				rasm_printf(ae,"[%s] Error line %d - syntax is SLA (IX+n),reg8\n",GetCurrentFile(ae),ae->wl[ae->idx].l);
				MaxError(ae);
		}
		ae->idx++;
		ae->idx++;
	}
}

void _SRA(struct s_assenv *ae) {
	/* on check qu'il y a un ou deux paramÃ¨tres */
	if (ae->wl[ae->idx+1].t==1) {
		switch (GetCRC(ae->wl[ae->idx+1].w)) {
			case CRC_B:___output(ae,0xCB);___output(ae,0x28);break;
			case CRC_C:___output(ae,0xCB);___output(ae,0x29);break;
			case CRC_D:___output(ae,0xCB);___output(ae,0x2A);break;
			case CRC_E:___output(ae,0xCB);___output(ae,0x2B);break;
			case CRC_H:___output(ae,0xCB);___output(ae,0x2C);break;
			case CRC_L:___output(ae,0xCB);___output(ae,0x2D);break;
			case CRC_MHL:___output(ae,0xCB);___output(ae,0x2E);break;
			case CRC_A:___output(ae,0xCB);___output(ae,0x2F);break;
			default:
				if (strncmp(ae->wl[ae->idx+1].w,"(IX",3)==0) {
					___output(ae,0xDD);___output(ae,0xCB);
					PushExpression(ae,ae->idx+1,E_EXPRESSION_IV8);
					___output(ae,0x2E);
				} else if (strncmp(ae->wl[ae->idx+1].w,"(IY",3)==0) {
					___output(ae,0xFD);___output(ae,0xCB);
					PushExpression(ae,ae->idx+1,E_EXPRESSION_IV8);
					___output(ae,0x2E);
				} else {
					rasm_printf(ae,"[%s] Error line %d - syntax is SRA reg8/(HL)/(IX+n)/(IY+n)\n",GetCurrentFile(ae),ae->wl[ae->idx].l);
					MaxError(ae);
				}
		}
		ae->idx++;
	} else if (!ae->wl[ae->idx+1].t && ae->wl[ae->idx+2].t!=2) {
		if (strncmp(ae->wl[ae->idx+1].w,"(IX",3)==0) {
			___output(ae,0xDD);
		} else if (strncmp(ae->wl[ae->idx+1].w,"(IY",3)==0) {
			___output(ae,0xFD);
		} else {
			rasm_printf(ae,"[%s] Error line %d - syntax is SRA (IX+n),reg8\n",GetCurrentFile(ae),ae->wl[ae->idx].l);
			MaxError(ae);
		}
		___output(ae,0xCB);
		switch (GetCRC(ae->wl[ae->idx+2].w)) {
			case CRC_B:PushExpression(ae,ae->idx+1,E_EXPRESSION_IV8);___output(ae,0x28);break;
			case CRC_C:PushExpression(ae,ae->idx+1,E_EXPRESSION_IV8);___output(ae,0x29);break;
			case CRC_D:PushExpression(ae,ae->idx+1,E_EXPRESSION_IV8);___output(ae,0x2A);break;
			case CRC_E:PushExpression(ae,ae->idx+1,E_EXPRESSION_IV8);___output(ae,0x2B);break;
			case CRC_H:PushExpression(ae,ae->idx+1,E_EXPRESSION_IV8);___output(ae,0x2C);break;
			case CRC_L:PushExpression(ae,ae->idx+1,E_EXPRESSION_IV8);___output(ae,0x2D);break;
			case CRC_A:PushExpression(ae,ae->idx+1,E_EXPRESSION_IV8);___output(ae,0x2F);break;
			default:			
				rasm_printf(ae,"[%s] Error line %d - syntax is SRA (IX+n),reg8\n",GetCurrentFile(ae),ae->wl[ae->idx].l);
				MaxError(ae);
		}
		ae->idx++;
		ae->idx++;
	}
}


void _SLL(struct s_assenv *ae) {
	/* on check qu'il y a un ou deux paramÃ¨tres */
	if (ae->wl[ae->idx+1].t==1) {
		switch (GetCRC(ae->wl[ae->idx+1].w)) {
			case CRC_B:___output(ae,0xCB);___output(ae,0x30);break;
			case CRC_C:___output(ae,0xCB);___output(ae,0x31);break;
			case CRC_D:___output(ae,0xCB);___output(ae,0x32);break;
			case CRC_E:___output(ae,0xCB);___output(ae,0x33);break;
			case CRC_H:___output(ae,0xCB);___output(ae,0x34);break;
			case CRC_L:___output(ae,0xCB);___output(ae,0x35);break;
			case CRC_MHL:___output(ae,0xCB);___output(ae,0x36);break;
			case CRC_A:___output(ae,0xCB);___output(ae,0x37);break;
			default:
				if (strncmp(ae->wl[ae->idx+1].w,"(IX",3)==0) {
					___output(ae,0xDD);___output(ae,0xCB);
					PushExpression(ae,ae->idx+1,E_EXPRESSION_IV8);
					___output(ae,0x36);
				} else if (strncmp(ae->wl[ae->idx+1].w,"(IY",3)==0) {
					___output(ae,0xFD);___output(ae,0xCB);
					PushExpression(ae,ae->idx+1,E_EXPRESSION_IV8);
					___output(ae,0x36);
				} else {
					rasm_printf(ae,"[%s] Error line %d - syntax is SLL reg8/(HL)/(IX+n)/(IY+n)\n",GetCurrentFile(ae),ae->wl[ae->idx].l);
					MaxError(ae);
				}
		}
		ae->idx++;
	} else if (!ae->wl[ae->idx+1].t && ae->wl[ae->idx+2].t!=2) {
		if (strncmp(ae->wl[ae->idx+1].w,"(IX",3)==0) {
			___output(ae,0xDD);
		} else if (strncmp(ae->wl[ae->idx+1].w,"(IY",3)==0) {
			___output(ae,0xFD);
		} else {
			rasm_printf(ae,"[%s] Error line %d - syntax is SLL (IX+n),reg8\n",GetCurrentFile(ae),ae->wl[ae->idx].l);
			MaxError(ae);
		}
		___output(ae,0xCB);
		switch (GetCRC(ae->wl[ae->idx+2].w)) {
			case CRC_B:PushExpression(ae,ae->idx+1,E_EXPRESSION_IV8);___output(ae,0x30);break;
			case CRC_C:PushExpression(ae,ae->idx+1,E_EXPRESSION_IV8);___output(ae,0x31);break;
			case CRC_D:PushExpression(ae,ae->idx+1,E_EXPRESSION_IV8);___output(ae,0x32);break;
			case CRC_E:PushExpression(ae,ae->idx+1,E_EXPRESSION_IV8);___output(ae,0x33);break;
			case CRC_H:PushExpression(ae,ae->idx+1,E_EXPRESSION_IV8);___output(ae,0x34);break;
			case CRC_L:PushExpression(ae,ae->idx+1,E_EXPRESSION_IV8);___output(ae,0x35);break;
			case CRC_A:PushExpression(ae,ae->idx+1,E_EXPRESSION_IV8);___output(ae,0x37);break;
			default:			
				rasm_printf(ae,"[%s] Error line %d - syntax is SLL (IX+n),reg8\n",GetCurrentFile(ae),ae->wl[ae->idx].l);
				MaxError(ae);
		}
		ae->idx++;
		ae->idx++;
	}
}

void _SRL(struct s_assenv *ae) {
	/* on check qu'il y a un ou deux paramÃ¨tres */
	if (ae->wl[ae->idx+1].t==1) {
		switch (GetCRC(ae->wl[ae->idx+1].w)) {
			case CRC_B:___output(ae,0xCB);___output(ae,0x38);break;
			case CRC_C:___output(ae,0xCB);___output(ae,0x39);break;
			case CRC_D:___output(ae,0xCB);___output(ae,0x3A);break;
			case CRC_E:___output(ae,0xCB);___output(ae,0x3B);break;
			case CRC_H:___output(ae,0xCB);___output(ae,0x3C);break;
			case CRC_L:___output(ae,0xCB);___output(ae,0x3D);break;
			case CRC_MHL:___output(ae,0xCB);___output(ae,0x3E);break;
			case CRC_A:___output(ae,0xCB);___output(ae,0x3F);break;
			default:
				if (strncmp(ae->wl[ae->idx+1].w,"(IX",3)==0) {
					___output(ae,0xDD);___output(ae,0xCB);
					PushExpression(ae,ae->idx+1,E_EXPRESSION_IV8);
					___output(ae,0x3E);
				} else if (strncmp(ae->wl[ae->idx+1].w,"(IY",3)==0) {
					___output(ae,0xFD);___output(ae,0xCB);
					PushExpression(ae,ae->idx+1,E_EXPRESSION_IV8);
					___output(ae,0x3E);
				} else {
					rasm_printf(ae,"[%s] Error line %d - syntax is SRL reg8/(HL)/(IX+n)/(IY+n)\n",GetCurrentFile(ae),ae->wl[ae->idx].l);
					MaxError(ae);
				}
		}
		ae->idx++;
	} else if (!ae->wl[ae->idx+1].t && ae->wl[ae->idx+2].t!=2) {
		if (strncmp(ae->wl[ae->idx+1].w,"(IX",3)==0) {
			___output(ae,0xDD);
		} else if (strncmp(ae->wl[ae->idx+1].w,"(IY",3)==0) {
			___output(ae,0xFD);
		} else {
			rasm_printf(ae,"[%s] Error line %d - syntax is SRL (IX+n),reg8\n",GetCurrentFile(ae),ae->wl[ae->idx].l);
			MaxError(ae);
		}
		___output(ae,0xCB);
		switch (GetCRC(ae->wl[ae->idx+2].w)) {
			case CRC_B:PushExpression(ae,ae->idx+1,E_EXPRESSION_IV8);___output(ae,0x38);break;
			case CRC_C:PushExpression(ae,ae->idx+1,E_EXPRESSION_IV8);___output(ae,0x39);break;
			case CRC_D:PushExpression(ae,ae->idx+1,E_EXPRESSION_IV8);___output(ae,0x3A);break;
			case CRC_E:PushExpression(ae,ae->idx+1,E_EXPRESSION_IV8);___output(ae,0x3B);break;
			case CRC_H:PushExpression(ae,ae->idx+1,E_EXPRESSION_IV8);___output(ae,0x3C);break;
			case CRC_L:PushExpression(ae,ae->idx+1,E_EXPRESSION_IV8);___output(ae,0x3D);break;
			case CRC_A:PushExpression(ae,ae->idx+1,E_EXPRESSION_IV8);___output(ae,0x3F);break;
			default:			
				rasm_printf(ae,"[%s] Error line %d - syntax is SRL (IX+n),reg8\n",GetCurrentFile(ae),ae->wl[ae->idx].l);
				MaxError(ae);
		}
		ae->idx++;
		ae->idx++;
	}
}


void _BIT(struct s_assenv *ae) {
	int o;
	/* on check qu'il y a deux ou trois parametres */
	ExpressionFastTranslate(ae,&ae->wl[ae->idx+1].w,0);
	o=RoundComputeExpressionCore(ae,ae->wl[ae->idx+1].w,ae->codeadr,0);
	if (o<0 || o>7) {
		rasm_printf(ae,"[%s] Error line %d - syntax is BIT <value from 0 to 7>,... (%d)\n",GetCurrentFile(ae),ae->wl[ae->idx].l,o);
		MaxError(ae);
	} else {
		o=0x40+o*8;
		if (ae->wl[ae->idx+1].t==0 && ae->wl[ae->idx+2].t==1) {
			switch (GetCRC(ae->wl[ae->idx+2].w)) {
				case CRC_B:___output(ae,0xCB);___output(ae,0x0+o);break;
				case CRC_C:___output(ae,0xCB);___output(ae,0x1+o);break;
				case CRC_D:___output(ae,0xCB);___output(ae,0x2+o);break;
				case CRC_E:___output(ae,0xCB);___output(ae,0x3+o);break;
				case CRC_H:___output(ae,0xCB);___output(ae,0x4+o);break;
				case CRC_L:___output(ae,0xCB);___output(ae,0x5+o);break;
				case CRC_MHL:___output(ae,0xCB);___output(ae,0x6+o);break;
				case CRC_A:___output(ae,0xCB);___output(ae,0x7+o);break;
				default:
					if (strncmp(ae->wl[ae->idx+2].w,"(IX",3)==0) {
						___output(ae,0xDD);___output(ae,0xCB);
						PushExpression(ae,ae->idx+2,E_EXPRESSION_IV8);
						___output(ae,0x6+o);
					} else if (strncmp(ae->wl[ae->idx+2].w,"(IY",3)==0) {
						___output(ae,0xFD);___output(ae,0xCB);
						PushExpression(ae,ae->idx+2,E_EXPRESSION_IV8);
						___output(ae,0x6+o);
					} else {
						rasm_printf(ae,"[%s] Error line %d - syntax is BIT n,reg8/(HL)/(IX+n)/(IY+n)\n",GetCurrentFile(ae),ae->wl[ae->idx].l);
						MaxError(ae);
					}
			}
			ae->idx+=2;
		} else if (!ae->wl[ae->idx+1].t && !ae->wl[ae->idx+2].t && ae->wl[ae->idx+3].t==1) {
			if (strncmp(ae->wl[ae->idx+2].w,"(IX",3)==0) {
				___output(ae,0xDD);
			} else if (strncmp(ae->wl[ae->idx+2].w,"(IY",3)==0) {
				___output(ae,0xFD);
			} else {
				rasm_printf(ae,"[%s] Error line %d - syntax is BIT (IX+n),reg8\n",GetCurrentFile(ae),ae->wl[ae->idx].l);
				MaxError(ae);
			}
			___output(ae,0xCB);
			switch (GetCRC(ae->wl[ae->idx+3].w)) {
				case CRC_B:PushExpression(ae,ae->idx+2,E_EXPRESSION_IV8);___output(ae,0x0+o);break;
				case CRC_C:PushExpression(ae,ae->idx+2,E_EXPRESSION_IV8);___output(ae,0x1+o);break;
				case CRC_D:PushExpression(ae,ae->idx+2,E_EXPRESSION_IV8);___output(ae,0x2+o);break;
				case CRC_E:PushExpression(ae,ae->idx+2,E_EXPRESSION_IV8);___output(ae,0x3+o);break;
				case CRC_H:PushExpression(ae,ae->idx+2,E_EXPRESSION_IV8);___output(ae,0x4+o);break;
				case CRC_L:PushExpression(ae,ae->idx+2,E_EXPRESSION_IV8);___output(ae,0x5+o);break;
				case CRC_A:PushExpression(ae,ae->idx+2,E_EXPRESSION_IV8);___output(ae,0x7+o);break;
				default:			
					rasm_printf(ae,"[%s] Error line %d - syntax is BIT n,(IX+n),reg8\n",GetCurrentFile(ae),ae->wl[ae->idx].l);
					MaxError(ae);
			}
			ae->idx+=3;
		} else {
			rasm_printf(ae,"[%s] Error line %d - syntax is BIT n,reg8/(HL)/(IX+n)[,reg8]/(IY+n)[,reg8]\n",GetCurrentFile(ae),ae->wl[ae->idx].l);
			MaxError(ae);
		}
	}
}

void _RES(struct s_assenv *ae) {
	int o;
	/* on check qu'il y a deux ou trois paramÃ¨tres */
	ExpressionFastTranslate(ae,&ae->wl[ae->idx+1].w,0);
	o=RoundComputeExpressionCore(ae,ae->wl[ae->idx+1].w,ae->codeadr,0);
	if (o<0 || o>7) {
		rasm_printf(ae,"[%s] Error line %d - syntax is RES <value from 0 to 7>,... (%d)\n",GetCurrentFile(ae),ae->wl[ae->idx].l,o);
		MaxError(ae);
	} else {
		o=0x80+o*8;
		if (ae->wl[ae->idx+1].t==0 && ae->wl[ae->idx+2].t==1) {
			switch (GetCRC(ae->wl[ae->idx+2].w)) {
				case CRC_B:___output(ae,0xCB);___output(ae,0x0+o);break;
				case CRC_C:___output(ae,0xCB);___output(ae,0x1+o);break;
				case CRC_D:___output(ae,0xCB);___output(ae,0x2+o);break;
				case CRC_E:___output(ae,0xCB);___output(ae,0x3+o);break;
				case CRC_H:___output(ae,0xCB);___output(ae,0x4+o);break;
				case CRC_L:___output(ae,0xCB);___output(ae,0x5+o);break;
				case CRC_MHL:___output(ae,0xCB);___output(ae,0x6+o);break;
				case CRC_A:___output(ae,0xCB);___output(ae,0x7+o);break;
				default:
					if (strncmp(ae->wl[ae->idx+2].w,"(IX",3)==0) {
						___output(ae,0xDD);___output(ae,0xCB);
						PushExpression(ae,ae->idx+2,E_EXPRESSION_IV8);
						___output(ae,0x6+o);
					} else if (strncmp(ae->wl[ae->idx+2].w,"(IY",3)==0) {
						___output(ae,0xFD);___output(ae,0xCB);
						PushExpression(ae,ae->idx+2,E_EXPRESSION_IV8);
						___output(ae,0x6+o);
					} else {
						rasm_printf(ae,"[%s] Error line %d - syntax is RES n,reg8/(HL)/(IX+n)/(IY+n)\n",GetCurrentFile(ae),ae->wl[ae->idx].l);
						MaxError(ae);
					}
			}
			ae->idx+=2;
		} else if (!ae->wl[ae->idx+1].t && !ae->wl[ae->idx+2].t && ae->wl[ae->idx+3].t==1) {
			if (strncmp(ae->wl[ae->idx+2].w,"(IX",3)==0) {
				___output(ae,0xDD);
			} else if (strncmp(ae->wl[ae->idx+2].w,"(IY",3)==0) {
				___output(ae,0xFD);
			} else {
				rasm_printf(ae,"[%s] Error line %d - syntax is RES n,(IX+n),reg8\n",GetCurrentFile(ae),ae->wl[ae->idx].l);
				MaxError(ae);
			}
			___output(ae,0xCB);
			switch (GetCRC(ae->wl[ae->idx+3].w)) {
				case CRC_B:PushExpression(ae,ae->idx+2,E_EXPRESSION_IV8);___output(ae,0x0+o);break;
				case CRC_C:PushExpression(ae,ae->idx+2,E_EXPRESSION_IV8);___output(ae,0x1+o);break;
				case CRC_D:PushExpression(ae,ae->idx+2,E_EXPRESSION_IV8);___output(ae,0x2+o);break;
				case CRC_E:PushExpression(ae,ae->idx+2,E_EXPRESSION_IV8);___output(ae,0x3+o);break;
				case CRC_H:PushExpression(ae,ae->idx+2,E_EXPRESSION_IV8);___output(ae,0x4+o);break;
				case CRC_L:PushExpression(ae,ae->idx+2,E_EXPRESSION_IV8);___output(ae,0x5+o);break;
				case CRC_A:PushExpression(ae,ae->idx+2,E_EXPRESSION_IV8);___output(ae,0x7+o);break;
				default:			
					rasm_printf(ae,"[%s] Error line %d - syntax is RES n,(IX+n),reg8\n",GetCurrentFile(ae),ae->wl[ae->idx].l);
					MaxError(ae);
			}
			ae->idx+=3;
		} else {
			rasm_printf(ae,"[%s] Error line %d - syntax is RES n,reg8/(HL)/(IX+n)[,reg8]/(IY+n)[,reg8]\n",GetCurrentFile(ae),ae->wl[ae->idx].l);
			MaxError(ae);
		}
	}
}

void _SET(struct s_assenv *ae) {
	int o;
	/* on check qu'il y a deux ou trois paramÃ¨tres */
	ExpressionFastTranslate(ae,&ae->wl[ae->idx+1].w,0);
	o=RoundComputeExpressionCore(ae,ae->wl[ae->idx+1].w,ae->codeadr,0);
	if (o<0 || o>7) {
		rasm_printf(ae,"[%s] Error line %d - syntax is SET <value from 0 to 7>,... (%d)\n",GetCurrentFile(ae),ae->wl[ae->idx].l,o);
		MaxError(ae);
	} else {
		o=0xC0+o*8;
		if (ae->wl[ae->idx+1].t==0 && ae->wl[ae->idx+2].t==1) {
			switch (GetCRC(ae->wl[ae->idx+2].w)) {
				case CRC_B:___output(ae,0xCB);___output(ae,0x0+o);break;
				case CRC_C:___output(ae,0xCB);___output(ae,0x1+o);break;
				case CRC_D:___output(ae,0xCB);___output(ae,0x2+o);break;
				case CRC_E:___output(ae,0xCB);___output(ae,0x3+o);break;
				case CRC_H:___output(ae,0xCB);___output(ae,0x4+o);break;
				case CRC_L:___output(ae,0xCB);___output(ae,0x5+o);break;
				case CRC_MHL:___output(ae,0xCB);___output(ae,0x6+o);break;
				case CRC_A:___output(ae,0xCB);___output(ae,0x7+o);break;
				default:
					if (strncmp(ae->wl[ae->idx+2].w,"(IX",3)==0) {
						___output(ae,0xDD);___output(ae,0xCB);
						PushExpression(ae,ae->idx+2,E_EXPRESSION_IV8);
						___output(ae,0x6+o);
					} else if (strncmp(ae->wl[ae->idx+2].w,"(IY",3)==0) {
						___output(ae,0xFD);___output(ae,0xCB);
						PushExpression(ae,ae->idx+2,E_EXPRESSION_IV8);
						___output(ae,0x6+o);
					} else {
						rasm_printf(ae,"[%s] Error line %d - syntax is SET n,reg8/(HL)/(IX+n)/(IY+n)\n",GetCurrentFile(ae),ae->wl[ae->idx].l);
						MaxError(ae);
					}
			}
			ae->idx+=2;
		} else if (!ae->wl[ae->idx+1].t && !ae->wl[ae->idx+2].t && ae->wl[ae->idx+3].t==1) {
			if (strncmp(ae->wl[ae->idx+2].w,"(IX",3)==0) {
				___output(ae,0xDD);
			} else if (strncmp(ae->wl[ae->idx+2].w,"(IY",3)==0) {
				___output(ae,0xFD);
			} else {
				rasm_printf(ae,"[%s] Error line %d - syntax is SET n,(IX+n),reg8\n",GetCurrentFile(ae),ae->wl[ae->idx].l);
				MaxError(ae);
			}
			___output(ae,0xCB);
			switch (GetCRC(ae->wl[ae->idx+3].w)) {
				case CRC_B:PushExpression(ae,ae->idx+2,E_EXPRESSION_IV8);___output(ae,0x0+o);break;
				case CRC_C:PushExpression(ae,ae->idx+2,E_EXPRESSION_IV8);___output(ae,0x1+o);break;
				case CRC_D:PushExpression(ae,ae->idx+2,E_EXPRESSION_IV8);___output(ae,0x2+o);break;
				case CRC_E:PushExpression(ae,ae->idx+2,E_EXPRESSION_IV8);___output(ae,0x3+o);break;
				case CRC_H:PushExpression(ae,ae->idx+2,E_EXPRESSION_IV8);___output(ae,0x4+o);break;
				case CRC_L:PushExpression(ae,ae->idx+2,E_EXPRESSION_IV8);___output(ae,0x5+o);break;
				case CRC_A:PushExpression(ae,ae->idx+2,E_EXPRESSION_IV8);___output(ae,0x7+o);break;
				default:			
					rasm_printf(ae,"[%s] Error line %d - syntax is SET n,(IX+n),reg8\n",GetCurrentFile(ae),ae->wl[ae->idx].l);
					MaxError(ae);
			}
			ae->idx+=3;
		} else {
			rasm_printf(ae,"[%s] Error line %d - syntax is SET n,reg8/(HL)/(IX+n)[,reg8]/(IY+n)[,reg8]\n",GetCurrentFile(ae),ae->wl[ae->idx].l);
			MaxError(ae);
		}
	}
}

void _DEFS(struct s_assenv *ae) {
	int i,r,v;
	if (!ae->wl[ae->idx].t && !ae->wl[ae->idx+1].t && ae->wl[ae->idx+2].t==1) {
		ExpressionFastTranslate(ae,&ae->wl[ae->idx+1].w,0);
		ExpressionFastTranslate(ae,&ae->wl[ae->idx+2].w,0);
		r=RoundComputeExpressionCore(ae,ae->wl[ae->idx+1].w,ae->codeadr,0);
		v=RoundComputeExpressionCore(ae,ae->wl[ae->idx+2].w,ae->codeadr,0);
		if (r<0) {
			rasm_printf(ae,"[%s] Error line %d - DEFS size must be greater or equal to zero\n",GetCurrentFile(ae),ae->wl[ae->idx].l);
			MaxError(ae);
		}
		for (i=0;i<r;i++) {
			___output(ae,v);
		}
		ae->idx+=2;
	} else if (!ae->wl[ae->idx].t && ae->wl[ae->idx+1].t==1) {
		ExpressionFastTranslate(ae,&ae->wl[ae->idx+1].w,0);
		r=RoundComputeExpressionCore(ae,ae->wl[ae->idx+1].w,ae->codeadr,0);
		v=0;
		if (r<0) {
			rasm_printf(ae,"[%s] Error line %d - DEFS size must be greater or equal to zero\n",GetCurrentFile(ae),ae->wl[ae->idx].l);
			MaxError(ae);
		}
		for (i=0;i<r;i++) {
			___output(ae,v);
		}
		ae->idx++;
	} else {
		rasm_printf(ae,"[%s] Error line %d - Syntax is DEFS repeat,value or DEFS repeat\n",GetCurrentFile(ae),ae->wl[ae->idx].l);
		MaxError(ae);
	}
}

void _STR(struct s_assenv *ae) {
	int i,tquote;

	if (!ae->wl[ae->idx].t) {
		do {
			ae->idx++;
			if ((tquote=StringIsQuote(ae->wl[ae->idx].w))!=0) {
				i=1;
				while (ae->wl[ae->idx].w[i] && ae->wl[ae->idx].w[i]!=tquote) {
					if (ae->wl[ae->idx].w[i]=='\\') i++;
					/* charset conversion on the fly */
					if (ae->wl[ae->idx].w[i+1]!=tquote) {
						___output(ae,ae->charset[(int)ae->wl[ae->idx].w[i]]);
					} else {
						___output(ae,ae->charset[(int)ae->wl[ae->idx].w[i]]|0x80);
					}
					i++;
				}
			} else {
				rasm_printf(ae,"[%s] Error line %d - STR handle only quoted strings!\n",GetCurrentFile(ae),ae->wl[ae->idx].l);
				MaxError(ae);
			}
		} while (ae->wl[ae->idx].t==0);
	} else {
		rasm_printf(ae,"[%s] Error line %d - STR needs one or more quotes parameters\n",GetCurrentFile(ae),ae->wl[ae->idx].l);
		MaxError(ae);
	}
}

void _DEFB(struct s_assenv *ae) {
	int i,tquote;

	if (!ae->wl[ae->idx].t) {
		do {
			ae->idx++;
			if ((tquote=StringIsQuote(ae->wl[ae->idx].w))!=0) {
				i=1;
				while (ae->wl[ae->idx].w[i] && ae->wl[ae->idx].w[i]!=tquote) {
					if (ae->wl[ae->idx].w[i]=='\\') i++;
					/* charset conversion on the fly */
					___output(ae,ae->charset[(int)ae->wl[ae->idx].w[i]]);
					i++;
				}
			} else {
				PushExpression(ae,ae->idx,E_EXPRESSION_0V8);
			}
		} while (ae->wl[ae->idx].t==0);
	} else {
		rasm_printf(ae,"[%s] Error line %d - DEFB needs one or more parameters\n",GetCurrentFile(ae),ae->wl[ae->idx].l);
		MaxError(ae);
	}
}

void _DEFW(struct s_assenv *ae) {
	if (!ae->wl[ae->idx].t) {
		do {
			ae->idx++;
			PushExpression(ae,ae->idx,E_EXPRESSION_0V16);
		} while (ae->wl[ae->idx].t==0);
	} else {
		rasm_printf(ae,"[%s] Error line %d - DEFW needs one or more parameters\n",GetCurrentFile(ae),ae->wl[ae->idx].l);
		MaxError(ae);
	}
}

void _DEFI(struct s_assenv *ae) {
	if (!ae->wl[ae->idx].t) {
		do {
			ae->idx++;
			PushExpression(ae,ae->idx,E_EXPRESSION_0V32);
		} while (ae->wl[ae->idx].t==0);
	} else {
		rasm_printf(ae,"[%s] Error line %d - DEFI needs one or more parameters\n",GetCurrentFile(ae),ae->wl[ae->idx].l);
		MaxError(ae);
	}
}


#undef FUNC
#define FUNC "Directive CORE"

void __internal_UpdateLZBlockIfAny(struct s_assenv *ae) {
	/* there was a crunched block opened in the previous bank */
	if (ae->lz>=0) {
		//ae->lzsection[ae->ilz-1].iorgzone=ae->io-1;
		//ae->lzsection[ae->ilz-1].ibank=ae->activebank;
	}
	ae->lz=-1;
}


void __AMSDOS(struct s_assenv *ae) {
	ae->amsdos=1;
}

void __BUILDCPR(struct s_assenv *ae) {
	ae->forcecpr=1;
}

void __LZ4(struct s_assenv *ae) {
	struct s_lz_section curlz;
	
	#ifdef NO_3RD_PARTIES
		rasm_printf(ae,"[%s] Error line %d - Cannot use 3rd parties cruncher with this version of RASM\n",GetCurrentFile(ae),ae->wl[ae->idx].l);
		exit(-5);
	#endif
	
	if (ae->lz>=0 && ae->lz<ae->ilz) {
		rasm_printf(ae,"[%s] Error line %d - Cannot start a new LZ section inside another one (%d)\n",GetCurrentFile(ae),ae->wl[ae->idx].l,ae->lz);
		exit(-5);
	}
	curlz.iw=ae->idx;
	curlz.iorgzone=ae->io-1;
	curlz.ibank=ae->activebank;
	curlz.memstart=ae->outputadr;
	curlz.memend=-1;
	curlz.lzversion=4;
	ae->lz=ae->ilz;
	ObjectArrayAddDynamicValueConcat((void**)&ae->lzsection,&ae->ilz,&ae->mlz,&curlz,sizeof(curlz));
}
void __LZX7(struct s_assenv *ae) {
	struct s_lz_section curlz;
	
	#ifdef NO_3RD_PARTIES
		rasm_printf(ae,"[%s] Error line %d - Cannot use 3rd parties cruncher with this version of RASM\n",GetCurrentFile(ae),ae->wl[ae->idx].l);
		exit(-5);
	#endif
	
	if (ae->lz>=0 && ae->lz<ae->ilz) {
		rasm_printf(ae,"[%s] Error line %d - Cannot start a new LZ section inside another one (%d)\n",GetCurrentFile(ae),ae->wl[ae->idx].l,ae->lz);
		exit(-5);
	}
	curlz.iw=ae->idx;
	curlz.iorgzone=ae->io-1;
	curlz.ibank=ae->activebank;
	curlz.memstart=ae->outputadr;
	curlz.memend=-1;
	curlz.lzversion=7;
	ae->lz=ae->ilz;
	ObjectArrayAddDynamicValueConcat((void**)&ae->lzsection,&ae->ilz,&ae->mlz,&curlz,sizeof(curlz));
}
void __LZEXO(struct s_assenv *ae) {
	struct s_lz_section curlz;
	
	#ifdef NO_3RD_PARTIES
		rasm_printf(ae,"[%s] Error line %d - Cannot use 3rd parties cruncher with this version of RASM\n",GetCurrentFile(ae),ae->wl[ae->idx].l);
		exit(-5);
	#endif
	
	if (ae->lz>=0 && ae->lz<ae->ilz) {
		rasm_printf(ae,"[%s] Error line %d - Cannot start a new LZ section inside another one (%d)\n",GetCurrentFile(ae),ae->wl[ae->idx].l,ae->lz);
		exit(-5);
	}
	curlz.iw=ae->idx;
	curlz.iorgzone=ae->io-1;
	curlz.ibank=ae->activebank;
	curlz.memstart=ae->outputadr;
	curlz.memend=-1;
	curlz.lzversion=8;
	ae->lz=ae->ilz;
	ObjectArrayAddDynamicValueConcat((void**)&ae->lzsection,&ae->ilz,&ae->mlz,&curlz,sizeof(curlz));
}
void __LZ48(struct s_assenv *ae) {
	struct s_lz_section curlz;
	
	if (ae->lz>=0 && ae->lz<ae->ilz) {
		rasm_printf(ae,"[%s] Error line %d - Cannot start a new LZ section inside another one (%d)\n",GetCurrentFile(ae),ae->wl[ae->idx].l,ae->lz);
		exit(-5);
	}
	curlz.iw=ae->idx;
	curlz.iorgzone=ae->io-1;
	curlz.ibank=ae->activebank;
	curlz.memstart=ae->outputadr;
	curlz.memend=-1;
	curlz.lzversion=48;
	ae->lz=ae->ilz;
	ObjectArrayAddDynamicValueConcat((void**)&ae->lzsection,&ae->ilz,&ae->mlz,&curlz,sizeof(curlz));
}
void __LZ49(struct s_assenv *ae) {
	struct s_lz_section curlz;
	
	if (ae->lz>=0 && ae->lz<ae->ilz) {
		rasm_printf(ae,"[%s] Error line %d - Cannot start a new LZ section inside another one (%d)\n",GetCurrentFile(ae),ae->wl[ae->idx].l,ae->lz);
		exit(-5);
	}
	
	curlz.iw=ae->idx;
	curlz.iorgzone=ae->io-1;
	curlz.ibank=ae->activebank;
	curlz.memstart=ae->outputadr;
	curlz.memend=-1;
	curlz.lzversion=49;
	ae->lz=ae->ilz;
	ObjectArrayAddDynamicValueConcat((void**)&ae->lzsection,&ae->ilz,&ae->mlz,&curlz,sizeof(curlz));
}
void __LZCLOSE(struct s_assenv *ae) {
	if (!ae->ilz || ae->lz==-1) {
		rasm_printf(ae,"[%s] Error line %d - Cannot close LZ section as it wasn't opened\n",GetCurrentFile(ae),ae->wl[ae->idx].l);
		MaxError(ae);
		return;
	}
	
	ae->lzsection[ae->ilz-1].memend=ae->outputadr;
	ae->lzsection[ae->ilz-1].ilabel=ae->il;
	ae->lzsection[ae->ilz-1].iexpr=ae->ie;
	//ae->lz=ae->ilz;
	ae->lz=-1;
}

void __LIMIT(struct s_assenv *ae) {
	if (ae->wl[ae->idx+1].t!=2) {
		ExpressionFastTranslate(ae,&ae->wl[ae->idx+1].w,0);
		___output_set_limit(ae,RoundComputeExpression(ae,ae->wl[ae->idx+1].w,ae->outputadr,0,0));
		ae->idx++;
	} else {
		rasm_printf(ae,"[%s] Error line %d - LIMIT directive need one integer parameter\n",GetCurrentFile(ae),ae->wl[ae->idx].l);
		MaxError(ae);
	}
}
void OverWriteCheck(struct s_assenv *ae)
{
	#undef FUNC
	#define FUNC "OverWriteCheck"
	
	int i,j;
	
	/* overwrite checking */
	i=ae->io-1; {
		if (ae->orgzone[i].memstart!=ae->orgzone[i].memend) {
			for (j=0;j<ae->io-1;j++) {
				if (ae->orgzone[j].memstart!=ae->orgzone[j].memend) {
					if (ae->orgzone[i].ibank==ae->orgzone[j].ibank) {
						if ((ae->orgzone[i].memstart>=ae->orgzone[j].memstart && ae->orgzone[i].memstart<ae->orgzone[j].memend)
							|| (ae->orgzone[i].memend>ae->orgzone[j].memstart && ae->orgzone[i].memend<ae->orgzone[j].memend)
							|| (ae->orgzone[i].memstart<=ae->orgzone[j].memstart && ae->orgzone[i].memend>=ae->orgzone[j].memend)) {
							ae->idx--;
							if (ae->orgzone[j].protect) {
								rasm_printf(ae,"PROTECTED section error",GetCurrentFile(ae),ae->wl[ae->idx].l,ae->orgzone[j].memstart,ae->orgzone[j].memend,ae->orgzone[j].ibank<32?ae->orgzone[j].ibank:0);
							} else {
								rasm_printf(ae,"Assembling overwrite ",GetCurrentFile(ae),ae->wl[ae->idx].l,ae->orgzone[j].memstart,ae->orgzone[j].memend);
							}
							rasm_printf(ae," [%s] L%d [#%04X-#%04X-B%d] with [%s] L%d [#%04X/#%04X]\n",ae->filename[ae->orgzone[j].ifile],ae->orgzone[j].iline,ae->orgzone[j].memstart,ae->orgzone[j].memend,ae->orgzone[j].ibank<32?ae->orgzone[j].ibank:0,ae->filename[ae->orgzone[i].ifile],ae->orgzone[i].iline,ae->orgzone[i].memstart,ae->orgzone[i].memend);
							MaxError(ae);
							i=j=ae->io;
							break;
						}
					}
				}
			}
		}
	}	
}


void __BANK(struct s_assenv *ae) {
	struct s_orgzone orgzone={0};

	__internal_UpdateLZBlockIfAny(ae);

	if (ae->io) {
		ae->orgzone[ae->io-1].memend=ae->outputadr;
	}
	ae->cprmode=1;
	if (ae->wl[ae->idx+1].t!=2) {
		ExpressionFastTranslate(ae,&ae->wl[ae->idx+1].w,0);
		ae->activebank=RoundComputeExpression(ae,ae->wl[ae->idx+1].w,ae->codeadr,0,0);
		if (ae->activebank<0 || ae->activebank>31) {
			rasm_printf(ae,"[%s] FATAL Error line %d - Bank selection must be from 0 to 31\n",GetCurrentFile(ae),ae->wl[ae->idx].l);
			exit(2);
		}
		ae->idx++;
	} else {
		rasm_printf(ae,"[%s] Error line %d - BANK directive need one integer parameter\n",GetCurrentFile(ae),ae->wl[ae->idx].l);
		MaxError(ae);
	}
	if (ae->lz>=0) {
		rasm_printf(ae,"[%s] Warning line %d - LZ section wasn't closed before a new BANK directive\n",GetCurrentFile(ae),ae->wl[ae->idx].l);
		__LZCLOSE(ae);
	}

	ae->outputadr=0;
	ae->codeadr=0;
	orgzone.memstart=0;
	orgzone.ibank=ae->activebank;
	ObjectArrayAddDynamicValueConcat((void**)&ae->orgzone,&ae->io,&ae->mo,&orgzone,sizeof(orgzone));

	OverWriteCheck(ae);
}

void __NameBANK(struct s_assenv *ae) {
	struct s_orgzone orgzone={0};
	int ibank;

	ae->cprmode=1;
	if (!ae->wl[ae->idx].t && !ae->wl[ae->idx+1].t && ae->wl[ae->idx+2].t==1) {
		if (!StringIsQuote(ae->wl[ae->idx+2].w)) {
			rasm_printf(ae,"[%s] Error line %d - Syntax is NAMEBANK <bank number>,'<string>'\n",GetCurrentFile(ae),ae->wl[ae->idx].l);
			MaxError(ae);
		} else {
			ExpressionFastTranslate(ae,&ae->wl[ae->idx+1].w,0);
			ibank=RoundComputeExpression(ae,ae->wl[ae->idx+1].w,ae->codeadr,0,0);
			if (ibank<0 || ibank>31) {
				rasm_printf(ae,"[%s] Error line %d - NAMEBANK selection must be from 0 to 31\n",GetCurrentFile(ae),ae->wl[ae->idx].l);
				MaxError(ae);
			} else {
				ae->iwnamebank[ibank]=ae->idx+2;
			}
		}
		ae->idx+=2;
	} else {
		rasm_printf(ae,"[%s] Error line %d - NAMEBANK directive need one integer parameter and a string\n",GetCurrentFile(ae),ae->wl[ae->idx].l);
		MaxError(ae);
	}
}

/***
	Winape little compatibility for CPR writing!
*/
void __WRITE(struct s_assenv *ae) {
	struct s_orgzone orgzone={0};
	unsigned char *mem;
	int idx=1,ok=0;
	int lower=-1,upper=-1,bank=-1;

	if (!ae->wl[ae->idx].t && strcmp(ae->wl[ae->idx+1].w,"DIRECT")==0 && !ae->wl[ae->idx+1].t) {
		ExpressionFastTranslate(ae,&ae->wl[ae->idx+2].w,0);
		lower=RoundComputeExpression(ae,ae->wl[ae->idx+2].w,ae->codeadr,0,0);
		if (!ae->wl[ae->idx+2].t) {
			ExpressionFastTranslate(ae,&ae->wl[ae->idx+3].w,0);
			upper=RoundComputeExpression(ae,ae->wl[ae->idx+3].w,ae->codeadr,0,0);
		}
		if (!ae->wl[ae->idx+3].t) {
			ExpressionFastTranslate(ae,&ae->wl[ae->idx+4].w,0);
			bank=RoundComputeExpression(ae,ae->wl[ae->idx+4].w,ae->codeadr,0,0);
		}

		if (ae->maxam) {
			if (lower==65535) lower=-1;
			if (upper==65535) upper=-1;
			if (bank==65535) bank=-1;
		}

		if (lower!=-1) {
			if (lower>=0 && lower<8) {
				ae->idx+=1;
				__BANK(ae);	
				ok=1;
			} else {
				rasm_printf(ae,"[%s] Warning line %d - WRITE DIRECT lower ROM ignored (value %d out of bounds 0-7)\n",GetCurrentFile(ae),ae->wl[ae->idx].l,lower);
			}
		} else if (upper!=-1) {
			if (upper>=0 && upper<32) {
				ae->idx+=2;
				__BANK(ae);	
				ok=1;
			} else {
				rasm_printf(ae,"[%s] Warning line %d - WRITE DIRECT upper ROM ignored (value %d out of bounds 0-31)\n",GetCurrentFile(ae),ae->wl[ae->idx].l,upper);
			}
		} else if (bank!=-1) {
			/* selection de bank on ouvre un nouvel espace */
		} else {
			rasm_printf(ae,"[%s] Warning line %d - meaningless WRITE DIRECT\n",GetCurrentFile(ae),ae->wl[ae->idx].l);
		}
	}
	while (!ae->wl[ae->idx].t) ae->idx++;
	if (!ok) {
		__internal_UpdateLZBlockIfAny(ae);
		if (ae->io) {
			ae->orgzone[ae->io-1].memend=ae->outputadr;
		}
		if (ae->lz>=0) {
			rasm_printf(ae,"[%s] Warning line %d - LZ section wasn't closed before a new BANK directive\n",GetCurrentFile(ae),ae->wl[ae->idx].l);
			__LZCLOSE(ae);
		}
		ae->activebank=ae->nbbank;
		mem=MemMalloc(65536);
		memset(mem,0,65536);
		ObjectArrayAddDynamicValueConcat((void**)&ae->mem,&ae->nbbank,&ae->maxbank,&mem,sizeof(mem));

		ae->outputadr=0;
		ae->codeadr=0;
		orgzone.memstart=0;
		orgzone.ibank=ae->activebank;
		ObjectArrayAddDynamicValueConcat((void**)&ae->orgzone,&ae->io,&ae->mo,&orgzone,sizeof(orgzone));

		OverWriteCheck(ae);
	}
}

void __CHARSET(struct s_assenv *ae) {
	int i,s,e,v,tquote;

	if (ae->wl[ae->idx].t==1) {
		/* reinit charset */
		for (i=0;i<256;i++)
			ae->charset[i]=i;
	} else if (!ae->wl[ae->idx].t && !ae->wl[ae->idx+1].t && ae->wl[ae->idx+2].t==1) {
		/* string,value | byte,value */
		ExpressionFastTranslate(ae,&ae->wl[ae->idx+2].w,0);
		v=RoundComputeExpression(ae,ae->wl[ae->idx+2].w,ae->codeadr,0,0);
		if (ae->wl[ae->idx+1].w[0]=='\'' || ae->wl[ae->idx+1].w[0]=='"') {
			tquote=ae->wl[ae->idx+1].w[0];
			if (ae->wl[ae->idx+1].w[strlen(ae->wl[ae->idx+1].w)-1]==tquote) {
				i=1;
				while (ae->wl[ae->idx+1].w[i] && ae->wl[ae->idx+1].w[i]!=tquote) {
					if (ae->wl[ae->idx+1].w[i]=='\\') i++;
					ae->charset[(int)ae->wl[ae->idx+1].w[i]]=(unsigned char)v++;
					i++;
				}
			} else {
				rasm_printf(ae,"[%s] Error line %d - CHARSET string,value has invalid quote!\n",GetCurrentFile(ae),ae->wl[ae->idx].l);
				MaxError(ae);
			}
		} else {
			i=RoundComputeExpression(ae,ae->wl[ae->idx+1].w,ae->codeadr,0,0);
			if (i>=0 && i<256) {
				ae->charset[i]=(unsigned char)v;
			} else {
				rasm_printf(ae,"[%s] Error line %d - CHARSET byte value must be 0-255\n",GetCurrentFile(ae),ae->wl[ae->idx].l);
				MaxError(ae);
			}
		}
		ae->idx+=2;
	} else if (!ae->wl[ae->idx].t && !ae->wl[ae->idx+1].t && !ae->wl[ae->idx+2].t && ae->wl[ae->idx+3].t==1) {
		/* start,end,value */
		ExpressionFastTranslate(ae,&ae->wl[ae->idx+1].w,0);
		ExpressionFastTranslate(ae,&ae->wl[ae->idx+2].w,0);
		ExpressionFastTranslate(ae,&ae->wl[ae->idx+3].w,0);
		s=RoundComputeExpression(ae,ae->wl[ae->idx+1].w,ae->codeadr,0,0);
		e=RoundComputeExpression(ae,ae->wl[ae->idx+2].w,ae->codeadr,0,0);
		v=RoundComputeExpression(ae,ae->wl[ae->idx+3].w,ae->codeadr,0,0);
		ae->idx+=3;
		if (s<=e && s>=0 && e<256) {
			for (i=s;i<=e;i++) {
				ae->charset[i]=(unsigned char)v++;
			}
		} else {
			rasm_printf(ae,"[%s] Error line %d - CHARSET winape directive wrong interval value\n",GetCurrentFile(ae),ae->wl[ae->idx].l);
			MaxError(ae);
		}
	} else {
		rasm_printf(ae,"[%s] Error line %d - CHARSET winape directive wrong parameter count\n",GetCurrentFile(ae),ae->wl[ae->idx].l);
		MaxError(ae);
	}
}

void __MACRO(struct s_assenv *ae) {
	struct s_macro curmacro={0};
	char *referentfilename,*zeparam;
	int refidx,idx,getparam=1;
	struct s_wordlist curwl;
	
	if (!ae->wl[ae->idx].t && ae->wl[ae->idx+1].t!=2) {
		/* get the name */
		curmacro.mnemo=ae->wl[ae->idx+1].w;
		curmacro.crc=GetCRC(curmacro.mnemo);
		if (ae->wl[ae->idx+1].t) {
			getparam=0;
		}
		idx=ae->idx+2;
		while (ae->wl[idx].t!=2 && (GetCRC(ae->wl[idx].w)!=CRC_MEND || strcmp(ae->wl[idx].w,"MEND")!=0)) {
			if (GetCRC(ae->wl[idx].w)==CRC_MACRO || strcmp(ae->wl[idx].w,"MACRO")==0) {
				/* inception interdite */
				referentfilename=GetCurrentFile(ae);
				refidx=ae->idx;
				ae->idx=idx;
				rasm_printf(ae,"[%s] Error line %d - You cannot define a macro inside another one (MACRO %s in [%s] L%d\n",GetCurrentFile(ae),ae->wl[idx].l,ae->wl[refidx+1].w,referentfilename,ae->wl[refidx].l);
				exit(-52);
			}
			if (getparam) {
				/* on prepare les parametres au remplacement */
				zeparam=MemMalloc(strlen(ae->wl[idx].w)+3);
				sprintf(zeparam,"{%s}",ae->wl[idx].w);
				curmacro.nbparam++;
				curmacro.param=MemRealloc(curmacro.param,curmacro.nbparam*sizeof(char **));
				curmacro.param[curmacro.nbparam-1]=zeparam;
				if (ae->wl[idx].t) {
					getparam=0;
				}
			} else {
				/* copie la liste de mots */	
				curwl=ae->wl[idx];
				ObjectArrayAddDynamicValueConcat((void **)&curmacro.wc,&curmacro.nbword,&curmacro.maxword,&curwl,sizeof(struct s_wordlist));
			}
			idx++;
		}
		if (ae->wl[idx].t==2) {
			rasm_printf(ae,"[%s] Error line %d - Macro was not closed\n",GetCurrentFile(ae),ae->wl[ae->idx].l);
			MaxError(ae);
		}
		ObjectArrayAddDynamicValueConcat((void**)&ae->macro,&ae->imacro,&ae->mmacro,&curmacro,sizeof(curmacro));
		/* le quicksort n'est pas optimal mais on n'est pas supposé en créer des milliers */
		qsort(ae->macro,ae->imacro,sizeof(struct s_macro),cmpmacros);

		/* ajustement des mots lus */
		if (ae->wl[idx].t==2) idx--;
		ae->idx=idx;
	} else {
		rasm_printf(ae,"[%s] Error line %d - MACRO definition need at least one parameter\n",GetCurrentFile(ae),ae->wl[ae->idx].l);
		MaxError(ae);
	}
}

struct s_wordlist *__MACRO_EXECUTE(struct s_assenv *ae, int imacro) {
	struct s_wordlist *cpybackup;
	int nbparam=0,idx,i,j,idad;
	int ifile,iline,iu;
	double v;
	struct s_macro_position curmacropos={0};
	char *zeparam=NULL;
	
	idx=ae->idx;
	while (!ae->wl[idx].t) {
		nbparam++;
		idx++;
	}
	if (nbparam!=ae->macro[imacro].nbparam) {
		rasm_printf(ae,"[%s] Error line %d - MACRO [%s] was defined with %d parameter%s\n",GetCurrentFile(ae),ae->wl[idx].l,ae->macro[imacro].mnemo,ae->macro[imacro].nbparam,ae->macro[imacro].nbparam>1?"s":"");
		MaxError(ae);
		ae->idx++;
	} else {
		/* free macro call as we will overwrite it */
		MemFree(ae->wl[ae->idx].w);
		/* eval parameters? */
		for (i=0;i<nbparam;i++) {
			if (strncmp(ae->wl[ae->idx+1+i].w,"{EVAL}",6)==0) {
				/* parametre entre chevrons, il faut l'interpreter d'abord */
				zeparam=TxtStrDup(ae->wl[ae->idx+1+i].w+6);
				ExpressionFastTranslate(ae,&zeparam,1);
				v=ComputeExpressionCore(ae,zeparam,ae->codeadr,0);
				MemFree(zeparam);
				zeparam=MemMalloc(32);
				snprintf(zeparam,31,"%lf",v);
				zeparam[31]=0;
				MemFree(ae->wl[ae->idx+1+i].w);
				ae->wl[ae->idx+1+i].w=zeparam;
			}
		}
		/* backup parameters */
		cpybackup=MemMalloc((nbparam+1)*sizeof(struct s_wordlist));
		for (i=0;i<nbparam;i++) {
			cpybackup[i]=ae->wl[ae->idx+1+i];
		}
		/* insert macro position */
		curmacropos.start=ae->idx;
		curmacropos.end=ae->idx+ae->macro[imacro].nbword;
		curmacropos.value=ae->macrocounter;
		ObjectArrayAddDynamicValueConcat((void**)&ae->macropos,&ae->imacropos,&ae->mmacropos,&curmacropos,sizeof(curmacropos));
		
		/* are we in a repeat/while block? */
		for (iu=0;iu<ae->ir;iu++) if (ae->repeat[iu].maxim<ae->imacropos) ae->repeat[iu].maxim=ae->imacropos;
		for (iu=0;iu<ae->iw;iu++) if (ae->whilewend[iu].maxim<ae->imacropos) ae->whilewend[iu].maxim=ae->imacropos;
		
		/* update daddy macropos */
		for (idad=0;idad<ae->imacropos-1;idad++) {
			if (ae->macropos[idad].end>curmacropos.start) {
				ae->macropos[idad].end+=ae->macro[imacro].nbword-1-nbparam; /* coz la macro compte un mot! */
			}
		}
		
#if 0
		for (idad=0;idad<ae->imacropos;idad++) {
			printf("macropos[%d]=%d -> %d\n",idad,ae->macropos[idad].start,ae->macropos[idad].end);
		}
#endif		
		/* insert at macro position and replace macro+parameters */
		if (ae->macro[imacro].nbword>1+nbparam) {
			ae->nbword+=ae->macro[imacro].nbword-1-nbparam;
			ae->wl=MemRealloc(ae->wl,ae->nbword*sizeof(struct s_wordlist));
		} else {
			/* si on réduit pas de realloc pour ne pas perdre de donnees */
			ae->nbword+=ae->macro[imacro].nbword-1-nbparam;
		}
		iline=ae->wl[ae->idx].l;
		ifile=ae->wl[ae->idx].ifile;
		MemMove(&ae->wl[ae->idx+ae->macro[imacro].nbword],&ae->wl[ae->idx+nbparam+1],(ae->nbword-ae->idx-ae->macro[imacro].nbword)*sizeof(struct s_wordlist));

		for (i=0;i<ae->macro[imacro].nbword;i++) {
			ae->wl[i+ae->idx].w=TxtStrDup(ae->macro[imacro].wc[i].w);
			ae->wl[i+ae->idx].l=iline;
			ae->wl[i+ae->idx].ifile=ifile;
			/* @@@sujet a evolution, ou double controle */
			ae->wl[i+ae->idx].t=ae->macro[imacro].wc[i].t;
			ae->wl[i+ae->idx].e=ae->macro[imacro].wc[i].e;
		}
		/* replace */
		idx=ae->idx;
		for (i=0;i<nbparam;i++) {
			for (j=idx;j<idx+ae->macro[imacro].nbword;j++) {
				ae->wl[j].w=TxtReplace(ae->wl[j].w,ae->macro[imacro].param[i],cpybackup[i].w,0);
			}
			MemFree(cpybackup[i].w);
		}
		MemFree(cpybackup);
		/* macro replaced, need to rollback index */
		//ae->idx--;
	}

	/* a chaque appel de macro on incremente le compteur pour les labels locaux */
	ae->macrocounter++;

	return ae->wl;
}

void __LET(struct s_assenv *ae) {
	if (!ae->wl[ae->idx].t && ae->wl[ae->idx+1].t==1) {
		ae->idx++;
		ExpressionFastTranslate(ae,&ae->wl[ae->idx].w,0);
		RoundComputeExpression(ae,ae->wl[ae->idx].w,ae->codeadr,0,0);
	} else {
		rasm_printf(ae,"[%s] Error line %d - LET useless winape directive need one expression\n",GetCurrentFile(ae),ae->wl[ae->idx].l);
		MaxError(ae);
	}
}

void __RUN(struct s_assenv *ae) {
	if (!ae->wl[ae->idx].t && ae->wl[ae->idx+1].t==1) {
		ae->idx++;
	} else {
		rasm_printf(ae,"[%s] Error line %d - RUN winape directive need one integer parameter\n",GetCurrentFile(ae),ae->wl[ae->idx].l);
		MaxError(ae);
	}
}

void __LIST(struct s_assenv *ae) {
	if (!ae->wl[ae->idx].t) {
		rasm_printf(ae,"[%s] Error line %d - LIST winape directive do not need parameter\n",GetCurrentFile(ae),ae->wl[ae->idx].l);
		MaxError(ae);
	}
}
void __NOLIST(struct s_assenv *ae) {
	if (!ae->wl[ae->idx].t) {
		rasm_printf(ae,"[%s] Error line %d - NOLIST winape directive do not need parameter\n",GetCurrentFile(ae),ae->wl[ae->idx].l);
		MaxError(ae);
	}
}

void __BRK(struct s_assenv *ae) {
	if (!ae->wl[ae->idx].t) {
		rasm_printf(ae,"[%s] Error line %d - BRK winape directive do not need parameter\n",GetCurrentFile(ae),ae->wl[ae->idx].l);
		MaxError(ae);
	}
}

void __STOP(struct s_assenv *ae) {
	if (!ae->wl[ae->idx].t) {
		rasm_printf(ae,"[%s] Error line %d - STOP winape directive do not need parameter. Anyway, STOP assembling...\n",GetCurrentFile(ae),ae->wl[ae->idx].l);
		MaxError(ae);
	}
	rasm_printf(ae,"stop assembling as requested\n");
	while (ae->wl[ae->idx].t!=2) ae->idx++;
	ae->idx--;
}

void __PRINT(struct s_assenv *ae) {
	while (ae->wl[ae->idx].t!=1) {
		if (!StringIsQuote(ae->wl[ae->idx+1].w)) {
			char *string2print=NULL;
			int hex=0,bin=0,entier=0;
			
			if (strncmp(ae->wl[ae->idx+1].w,"{HEX}",5)==0) {
				string2print=TxtStrDup(ae->wl[ae->idx+1].w+5);
				hex=1;
			} else if (strncmp(ae->wl[ae->idx+1].w,"{HEX2}",6)==0) {
				string2print=TxtStrDup(ae->wl[ae->idx+1].w+6);
				hex=2;
			} else if (strncmp(ae->wl[ae->idx+1].w,"{HEX4}",6)==0) {
				string2print=TxtStrDup(ae->wl[ae->idx+1].w+6);
				hex=4;
			} else if (strncmp(ae->wl[ae->idx+1].w,"{HEX8}",6)==0) {
				string2print=TxtStrDup(ae->wl[ae->idx+1].w+6);
				hex=8;
			} else if (strncmp(ae->wl[ae->idx+1].w,"{BIN}",5)==0) {
				string2print=TxtStrDup(ae->wl[ae->idx+1].w+5);
				bin=1;
			} else if (strncmp(ae->wl[ae->idx+1].w,"{BIN8}",6)==0) {
				string2print=TxtStrDup(ae->wl[ae->idx+1].w+6);
				bin=8;
			} else if (strncmp(ae->wl[ae->idx+1].w,"{BIN16}",7)==0) {
				string2print=TxtStrDup(ae->wl[ae->idx+1].w+7);
				bin=16;
			} else if (strncmp(ae->wl[ae->idx+1].w,"{BIN32}",7)==0) {
				string2print=TxtStrDup(ae->wl[ae->idx+1].w+7);
				bin=32;
			} else if (strncmp(ae->wl[ae->idx+1].w,"{INT}",5)==0) {
				string2print=TxtStrDup(ae->wl[ae->idx+1].w+5);
				entier=1;
			} else {
				string2print=TxtStrDup(ae->wl[ae->idx+1].w);
			}

			ExpressionFastTranslate(ae,&string2print,1);
			if (hex) {
				int zv;
				zv=RoundComputeExpressionCore(ae,string2print,ae->codeadr,0);
				switch (hex) {
					case 1:
						if (zv&0xFFFFFF00) {
							if (zv&0xFFFF0000) {
								rasm_printf(ae,"#%-8.08X ",zv);
							} else {
								rasm_printf(ae,"#%-4.04X ",zv);
							}
						} else {
							rasm_printf(ae,"#%-2.02X ",zv);
						}
						break;
					case 2:rasm_printf(ae,"#%-2.02X ",zv);break;
					case 4:rasm_printf(ae,"#%-4.04X ",zv);break;
					case 8:rasm_printf(ae,"#%-8.08X ",zv);break;
				}
			} else if (bin) {
				int zv,d;
				zv=RoundComputeExpressionCore(ae,string2print,ae->codeadr,0);
				/* remove useless sign bits */
				if (bin<32 && (zv&0xFFFF0000)==0xFFFF0000) {
					zv&=0xFFFF;
				}
				switch (bin) {
					case 1:if (zv&0xFF00) d=15; else d=7;break;
					case 8:d=7;break;
					case 16:d=15;break;
					case 32:d=31;break;
				}
				rasm_printf(ae,"%%");
				for (;d>=0;d--) {
					if ((zv>>d)&1) rasm_printf(ae,"1"); else rasm_printf(ae,"0");
				}
				rasm_printf(ae," ");
			} else if (entier) {
				rasm_printf(ae,"%d ",(int)RoundComputeExpressionCore(ae,string2print,ae->codeadr,0));
			} else {
				rasm_printf(ae,"%.2lf ",ComputeExpressionCore(ae,string2print,ae->codeadr,0));
			}
			MemFree(string2print);
		} else {
			int lm;
			lm=strlen(ae->wl[ae->idx+1].w)-2;
			if (lm) rasm_printf(ae,"%-*.*s ",lm,lm,ae->wl[ae->idx+1].w+1);
		}
		ae->idx++;
	}
	rasm_printf(ae,"\n");
}

void __ALIGN(struct s_assenv *ae) {
	int aval;
	
	if (ae->io) {
		ae->orgzone[ae->io-1].memend=ae->outputadr;
	}
	if (ae->wl[ae->idx+1].t!=2) {
		ExpressionFastTranslate(ae,&ae->wl[ae->idx+1].w,0);
		aval=RoundComputeExpression(ae,ae->wl[ae->idx+1].w,ae->codeadr,0,0);
		ae->idx++;

		if (aval<1 || aval>65535) {
			rasm_printf(ae,"[%s] Error line %d - ALIGN directive need one integer\n",GetCurrentFile(ae),ae->wl[ae->idx].l);
			MaxError(ae);
		}

		/* touch codeadr only if adress is misaligned */	
		if (ae->codeadr%aval) {
			/* move outputadr the same value as codeadr move */
			ae->outputadr=ae->outputadr-(ae->codeadr%aval)+aval;
			ae->codeadr=ae->codeadr-(ae->codeadr%aval)+aval;
		}
	} else {
		rasm_printf(ae,"[%s] Error line %d - ALIGN directive need one integer parameter\n",GetCurrentFile(ae),ae->wl[ae->idx].l);
		MaxError(ae);
	}
}

void __WHILE(struct s_assenv *ae) {
	struct s_whilewend whilewend={0};
	if (!ae->wl[ae->idx].t && ae->wl[ae->idx+1].t==1) {
		ae->idx++;
		whilewend.start=ae->idx;
		whilewend.cpt=0;
		whilewend.value=ae->whilecounter;
		whilewend.maxim=ae->imacropos;
		ae->whilecounter++;
		/* pour gérer les macros situés dans le while précedent après un repeat/while courant */
		if (ae->iw) whilewend.maxim=ae->whilewend[ae->iw-1].maxim;
		if (ae->ir && ae->repeat[ae->ir-1].maxim>whilewend.maxim) whilewend.maxim=ae->repeat[ae->ir-1].maxim;
		ObjectArrayAddDynamicValueConcat((void**)&ae->whilewend,&ae->iw,&ae->mw,&whilewend,sizeof(whilewend));
	} else {
		rasm_printf(ae,"[%s] Error line %d - syntax is WHILE <expression>\n",GetCurrentFile(ae),ae->wl[ae->idx].l);
		MaxError(ae);
	}
}
void __WEND(struct s_assenv *ae) {
	if (ae->iw>0) {
		if (ae->wl[ae->idx].t==1) {
			ExpressionFastTranslate(ae,&ae->wl[ae->idx+1].w,0);
			if (ComputeExpression(ae,ae->wl[ae->whilewend[ae->iw-1].start].w,ae->codeadr,0,2)) {
				ae->whilewend[ae->iw-1].cpt++; /* for local label */
				ae->idx=ae->whilewend[ae->iw-1].start;
				/* refresh macro check index */
				ae->imacropos=ae->whilewend[ae->iw-1].maxim;
			} else {
				ae->iw--;
				/* refresh macro check index */
				if (ae->iw) ae->imacropos=ae->whilewend[ae->iw-1].maxim;
			}
		} else {
			rasm_printf(ae,"[%s] Error line %d, WEND does not need any parameter\n",GetCurrentFile(ae),ae->wl[ae->idx].l);
			MaxError(ae);
		}
	} else {
		rasm_printf(ae,"[%s] Error line %d, WEND encounter whereas there is no referent WHILE\n",GetCurrentFile(ae),ae->wl[ae->idx].l);
		MaxError(ae);
	}
}

enum e_loop_style {
E_LOOPSTYLE_REPEATN,
E_LOOPSTYLE_REPEATUNTIL,
E_LOOPSTYLE_WHILE
};

void __REPEAT(struct s_assenv *ae) {
	struct s_repeat currepeat={0};
	int *loopstyle;
	int iloop,mloop;
	int cidx;
	
	if (ae->wl[ae->idx+1].t!=2) {
		if (ae->wl[ae->idx].t==0) {
			ExpressionFastTranslate(ae,&ae->wl[ae->idx+1].w,0);
			currepeat.cpt=RoundComputeExpression(ae,ae->wl[ae->idx+1].w,0,0,0);
			if (!currepeat.cpt) {
				/* skip repeat block */
				cidx=ae->idx+2;
				iloop=mloop=0;
				loopstyle=NULL;
				IntArrayAddDynamicValueConcat(&loopstyle,&iloop,&mloop,E_LOOPSTYLE_REPEATN);
				/* look for REND */
				while (iloop) {
					if (strcmp(ae->wl[cidx].w,"REPEAT")==0) {
						if (ae->wl[cidx].t) {
							IntArrayAddDynamicValueConcat(&loopstyle,&iloop,&mloop,E_LOOPSTYLE_REPEATUNTIL);
						} else if (ae->wl[cidx+1].t) {
							IntArrayAddDynamicValueConcat(&loopstyle,&iloop,&mloop,E_LOOPSTYLE_REPEATN);
						} else {
							rasm_printf(ae,"[%s] FATAL Error line %d - Invalid REPEAT syntax\n",GetCurrentFile(ae),ae->wl[ae->idx].l,currepeat.cpt);
							exit(7);
						}
					} else if (strcmp(ae->wl[cidx].w,"WHILE")==0) {
						if (!ae->wl[cidx].t && ae->wl[cidx+1].t) {
							IntArrayAddDynamicValueConcat(&loopstyle,&iloop,&mloop,E_LOOPSTYLE_WHILE);
						} else {
							rasm_printf(ae,"[%s] FATAL Error line %d - Invalid WHILE syntax\n",GetCurrentFile(ae),ae->wl[ae->idx].l,currepeat.cpt);
							exit(7);
						}
					} else if (strcmp(ae->wl[cidx].w,"WEND")==0) {
						iloop--;
						if (loopstyle[iloop]!=E_LOOPSTYLE_WHILE) {
							rasm_printf(ae,"[%s] FATAL Error line %d - WEND encountered but expecting %s\n",GetCurrentFile(ae),ae->wl[ae->idx].l,currepeat.cpt,loopstyle[iloop]==E_LOOPSTYLE_REPEATN?"REND":"UNTIL");
							exit(7);
						}
					} else if (strcmp(ae->wl[cidx].w,"REND")==0) {
						iloop--;
						if (loopstyle[iloop]!=E_LOOPSTYLE_REPEATN) {
							rasm_printf(ae,"[%s] FATAL Error line %d - REND encountered but expecting %s\n",GetCurrentFile(ae),ae->wl[ae->idx].l,currepeat.cpt,loopstyle[iloop]==E_LOOPSTYLE_REPEATUNTIL?"UNTIL":"WEND");
							exit(7);
						}
					} else if (strcmp(ae->wl[cidx].w,"UNTIL")==0) {
						iloop--;
						if (loopstyle[iloop]!=E_LOOPSTYLE_REPEATUNTIL) {
							rasm_printf(ae,"[%s] FATAL Error line %d - UNTIL encountered but expecting %s\n",GetCurrentFile(ae),ae->wl[ae->idx].l,currepeat.cpt,loopstyle[iloop]==E_LOOPSTYLE_REPEATN?"REND":"WEND");
							exit(7);
						}
					}
					while (!ae->wl[cidx].t) cidx++;
					cidx++;
				}
				MemFree(loopstyle);
				ae->idx=cidx-1;
				return;
			} else if (currepeat.cpt<1 || currepeat.cpt>65536) {
				rasm_printf(ae,"[%s] Error line %d - Repeat value (%d) must be from 1 to 65535\n",GetCurrentFile(ae),ae->wl[ae->idx].l,currepeat.cpt);
				exit(2);
			}
			ae->idx++;
			currepeat.start=ae->idx;
		} else {
			currepeat.start=ae->idx;
			currepeat.cpt=-1;
		}
		currepeat.value=ae->repeatcounter;
		ae->repeatcounter++;
		/* pour gérer les macros situés dans le repeat précedent après le repeat courant */
		if (ae->ir) currepeat.maxim=ae->repeat[ae->ir-1].maxim;
		if (ae->iw && ae->whilewend[ae->iw-1].maxim>currepeat.maxim) currepeat.maxim=ae->whilewend[ae->iw-1].maxim;
		if (ae->imacropos>currepeat.maxim) currepeat.maxim=ae->imacropos;
		ObjectArrayAddDynamicValueConcat((void**)&ae->repeat,&ae->ir,&ae->mr,&currepeat,sizeof(currepeat));
	} else {
		rasm_printf(ae,"[%s] Error line %d - wrong REPEAT usage\n",GetCurrentFile(ae),ae->wl[ae->idx].l);
		MaxError(ae);
	}
}

void __REND(struct s_assenv *ae) {
	if (ae->ir>0) {
		if (ae->repeat[ae->ir-1].cpt==-1) {
			rasm_printf(ae,"[%s] Error line %d, REND encounter whereas referent REPEAT was waiting for UNTIL\n",GetCurrentFile(ae),ae->wl[ae->idx].l);
			MaxError(ae);
		} else {
			ae->repeat[ae->ir-1].cpt--;
			if (ae->repeat[ae->ir-1].cpt) {
				ae->idx=ae->repeat[ae->ir-1].start;
				/* refresh macro check index */
				ae->imacropos=ae->repeat[ae->ir-1].maxim;
			} else {
				ae->ir--;
				/* refresh macro check index */
				if (ae->ir) ae->imacropos=ae->repeat[ae->ir-1].maxim;
			}
		}
	} else {
		rasm_printf(ae,"[%s] Error line %d, REND encounter whereas there is no referent REPEAT\n",GetCurrentFile(ae),ae->wl[ae->idx].l);
		MaxError(ae);
	}
}

void __UNTIL(struct s_assenv *ae) {
	if (ae->ir>0) {
		if (ae->repeat[ae->ir-1].cpt>=0) {
			rasm_printf(ae,"[%s] Error line %d, UNTIL encounter whereas referent REPEAT n was waiting for REND\n",GetCurrentFile(ae),ae->wl[ae->idx].l);
			MaxError(ae);
		} else {
			if (ae->wl[ae->idx].t==0 && ae->wl[ae->idx+1].t==1) {
				ExpressionFastTranslate(ae,&ae->wl[ae->idx+1].w,0);
				if (ComputeExpression(ae,ae->wl[ae->idx+1].w,ae->codeadr,0,2)) {
					ae->idx=ae->repeat[ae->ir-1].start;
					ae->repeat[ae->ir-1].cpt--; /* for local label */
					/* refresh macro check index */
					ae->imacropos=ae->repeat[ae->ir-1].maxim;
				} else {
					ae->ir--;
					/* refresh macro check index */
					if (ae->ir) ae->imacropos=ae->repeat[ae->ir-1].maxim;
				}
			} else {
				rasm_printf(ae,"[%s] Error line %d, UNTIL need one expression/evaluation as parameter\n",GetCurrentFile(ae),ae->wl[ae->idx].l);
				MaxError(ae);
			}
		}
	} else {
		rasm_printf(ae,"[%s] Error line %d, UNTIL encounter whereas there is no referent REPEAT\n",GetCurrentFile(ae),ae->wl[ae->idx].l);
		MaxError(ae);
	}
}

void __ASSERT(struct s_assenv *ae) {
	int rexpr;

	if (!ae->wl[ae->idx].t && ae->wl[ae->idx+1].t==1) {
		ExpressionFastTranslate(ae,&ae->wl[ae->idx+1].w,0);
		rexpr=!!RoundComputeExpression(ae,ae->wl[ae->idx+1].w,ae->codeadr,0,1);
		if (!rexpr) {
			rasm_printf(ae,"[%s] Error line %d, ASSERT failed!\n",GetCurrentFile(ae),ae->wl[ae->idx].l);
			exit(2);
		}
		ae->idx++;
	} else {
		rasm_printf(ae,"[%s] Error line %d, ASSERT need one expression\n",GetCurrentFile(ae),ae->wl[ae->idx].l);
		exit(2);
	}
}

void __IF(struct s_assenv *ae) {
	int rexpr;

	if (!ae->wl[ae->idx].t && ae->wl[ae->idx+1].t==1) {
		ExpressionFastTranslate(ae,&ae->wl[ae->idx+1].w,0);
		rexpr=!!RoundComputeExpression(ae,ae->wl[ae->idx+1].w,ae->codeadr,0,1);
		IntArrayAddDynamicValueConcat(&ae->ifthen,&ae->ii,&ae->mi,rexpr);
		ae->idx++;
	} else {
		rasm_printf(ae,"[%s] Error line %d, IF need one expression\n",GetCurrentFile(ae),ae->wl[ae->idx].l);
		exit(2);
	}
}

void __SWITCH(struct s_assenv *ae) {
	struct s_switchcase curswitch={0};

	if (!ae->wl[ae->idx].t && ae->wl[ae->idx+1].t==1) {
		/* switch store the value */
		ExpressionFastTranslate(ae,&ae->wl[ae->idx+1].w,0);
		curswitch.refval=RoundComputeExpression(ae,ae->wl[ae->idx+1].w,ae->codeadr,0,1);
		ObjectArrayAddDynamicValueConcat((void**)&ae->switchcase,&ae->isw,&ae->msw,&curswitch,sizeof(curswitch));
		ae->idx++;
	} else {
		rasm_printf(ae,"[%s] Error line %d, SWITCH need one expression\n",GetCurrentFile(ae),ae->wl[ae->idx].l);
		exit(2);
	}
}
void __CASE(struct s_assenv *ae) {
	int rexpr;
	
	if (ae->isw) {
		if (!ae->wl[ae->idx].t && ae->wl[ae->idx+1].t==1) {
			ExpressionFastTranslate(ae,&ae->wl[ae->idx+1].w,0);
			rexpr=RoundComputeExpression(ae,ae->wl[ae->idx+1].w,ae->codeadr,0,1);
			
			if (ae->switchcase[ae->isw-1].refval==rexpr) {
				ae->switchcase[ae->isw-1].execute=1;
				ae->switchcase[ae->isw-1].casematch=1;
			}
		} else {
			rasm_printf(ae,"[%s] Error line %d, CASE not need one parameter\n",GetCurrentFile(ae),ae->wl[ae->idx].l);
			MaxError(ae);
		}
	} else {
		rasm_printf(ae,"[%s] Error line %d, CASE encounter whereas there is no referent SWITCH\n",GetCurrentFile(ae),ae->wl[ae->idx].l);
		MaxError(ae);
	}
}
void __DEFAULT(struct s_assenv *ae) {
	
	if (ae->isw) {
		if (ae->wl[ae->idx].t==1) {
			/* aucun match avant, on active, sinon on laisse tel quel */
			if (!ae->switchcase[ae->isw-1].casematch) {
				ae->switchcase[ae->isw-1].execute=1;
			}
		} else {
			rasm_printf(ae,"[%s] Error line %d, DEFAULT do not need parameter\n",GetCurrentFile(ae),ae->wl[ae->idx].l);
			MaxError(ae);
		}
	} else {
		rasm_printf(ae,"[%s] Error line %d, DEFAULT encounter whereas there is no referent SWITCH\n",GetCurrentFile(ae),ae->wl[ae->idx].l);
		MaxError(ae);
	}
}
void __BREAK(struct s_assenv *ae) {
	
	if (ae->isw) {
		if (ae->wl[ae->idx].t==1) {
			ae->switchcase[ae->isw-1].execute=0;
		} else {
			rasm_printf(ae,"[%s] Error line %d, BREAK do not need parameter\n",GetCurrentFile(ae),ae->wl[ae->idx].l);
			MaxError(ae);
		}
	} else {
		rasm_printf(ae,"[%s] Error line %d, BREAK encounter whereas there is no referent SWITCH\n",GetCurrentFile(ae),ae->wl[ae->idx].l);
		MaxError(ae);
	}
}
void __ENDSWITCH(struct s_assenv *ae) {
	if (ae->isw) {
		if (ae->wl[ae->idx].t==1) {
			ae->isw--;
		} else {
			rasm_printf(ae,"[%s] Error line %d, ENDSWITCH does not need any parameter\n",GetCurrentFile(ae),ae->wl[ae->idx].l);
			MaxError(ae);
		}
	} else {
		rasm_printf(ae,"[%s] Error line %d, ENDSWITCH encounter whereas there is no referent SWITCH\n",GetCurrentFile(ae),ae->wl[ae->idx].l);
		MaxError(ae);
	}
}

void __IFNOT(struct s_assenv *ae) {
	int rexpr;

	if (!ae->wl[ae->idx].t && ae->wl[ae->idx+1].t==1) {
		ExpressionFastTranslate(ae,&ae->wl[ae->idx+1].w,0);
		rexpr=!RoundComputeExpression(ae,ae->wl[ae->idx+1].w,ae->codeadr,0,1);
		IntArrayAddDynamicValueConcat(&ae->ifthen,&ae->ii,&ae->mi,rexpr);
		ae->idx++;
	} else {
		rasm_printf(ae,"[%s] Error line %d, IFNOT need one expression\n",GetCurrentFile(ae),ae->wl[ae->idx].l);
		exit(2);
	}
}

void __ELSE(struct s_assenv *ae) {
	if (ae->ii) {
		if (ae->wl[ae->idx].t==1) {
			/* ELSE a executer seulement si celui d'avant est a zero */
			switch (ae->ifthen[ae->ii-1]) {
				case -1:break;
				case 0:ae->ifthen[ae->ii-1]=1;break;
				case 1:ae->ifthen[ae->ii-1]=0;break;
			}
		} else {
			rasm_printf(ae,"[%s] Error line %d, ELSE does not need any parameter\n",GetCurrentFile(ae),ae->wl[ae->idx].l);
			MaxError(ae);
		}
	} else {
		rasm_printf(ae,"[%s] Error line %d, ELSE encounter whereas there is no referent IF\n",GetCurrentFile(ae),ae->wl[ae->idx].l);
		MaxError(ae);
	}
}
void __ELSEIF(struct s_assenv *ae) {

	if (!ae->wl[ae->idx].t && ae->wl[ae->idx+1].t==1) {
		if (ae->ifthen[ae->ii-1]) {
			/* il faut signifier aux suivants qu'on va jusqu'au ENDIF */
			ae->ifthen[ae->ii-1]=-1;
		} else {
			ExpressionFastTranslate(ae,&ae->wl[ae->idx+1].w,0);
			ae->ifthen[ae->ii-1]=!!RoundComputeExpression(ae,ae->wl[ae->idx+1].w,ae->codeadr,0,1);
		}
		ae->idx++;
	} else {
		rasm_printf(ae,"[%s] Error line %d, ELSEIF need one expression\n",GetCurrentFile(ae),ae->wl[ae->idx].l);
		exit(2);
	}
}
void __ENDIF(struct s_assenv *ae) {
	if (ae->ii) {
		if (ae->wl[ae->idx].t==1) {
			ae->ii--;
		} else {
			rasm_printf(ae,"[%s] Error line %d, ENDIF does not need any parameter\n",GetCurrentFile(ae),ae->wl[ae->idx].l);
			MaxError(ae);
		}
	} else {
		rasm_printf(ae,"[%s] Error line %d, ENDIF encounter whereas there is no referent IF\n",GetCurrentFile(ae),ae->wl[ae->idx].l);
		MaxError(ae);
	}
}

void __PROTECT(struct s_assenv *ae) {
	struct s_orgzone orgzone={0};

	if (!ae->wl[ae->idx].t && !ae->wl[ae->idx+1].t && ae->wl[ae->idx+2].t==1) {
		/* add a fake ORG zone */
		ObjectArrayAddDynamicValueConcat((void**)&ae->orgzone,&ae->io,&ae->mo,&orgzone,sizeof(orgzone));
		/* then switch it with the current ORG */
		orgzone=ae->orgzone[ae->io-2];
		ExpressionFastTranslate(ae,&ae->wl[ae->idx+1].w,0);
		ExpressionFastTranslate(ae,&ae->wl[ae->idx+2].w,0);
		ae->orgzone[ae->io-2].memstart=RoundComputeExpression(ae,ae->wl[ae->idx+1].w,0,0,0);
		ae->orgzone[ae->io-2].memend=RoundComputeExpression(ae,ae->wl[ae->idx+2].w,0,0,0);
		ae->orgzone[ae->io-2].ibank=ae->activebank;
		ae->orgzone[ae->io-2].protect=1;
		ae->orgzone[ae->io-1]=orgzone;
		
	} else {
		rasm_printf(ae,"[%s] Error line %d, PROTECT need two parameters: startadr,endadr\n",GetCurrentFile(ae),ae->wl[ae->idx].l);
		MaxError(ae);
	}
}

void __ORG(struct s_assenv *ae) {
	struct s_orgzone orgzone={0};
	int i;

	if (ae->lz>=0) {
		rasm_printf(ae,"[%s] Error line %d - Cannot ORG inside a LZ section\n",GetCurrentFile(ae),ae->wl[ae->idx].l);
		exit(-5);
	}
	__internal_UpdateLZBlockIfAny(ae);
	
	/* close current ORG */
	if (ae->io) {
		ae->orgzone[ae->io-1].memend=ae->outputadr;
	}
	
	if (ae->wl[ae->idx+1].t!=2) {
		ExpressionFastTranslate(ae,&ae->wl[ae->idx+1].w,0);
		ae->codeadr=RoundComputeExpression(ae,ae->wl[ae->idx+1].w,ae->outputadr,0,0);
		if (!ae->wl[ae->idx+1].t && ae->wl[ae->idx+2].t!=2) {
			ExpressionFastTranslate(ae,&ae->wl[ae->idx+2].w,0);
			ae->outputadr=RoundComputeExpression(ae,ae->wl[ae->idx+2].w,ae->outputadr,0,0);
			ae->idx+=2;
		} else {
			ae->outputadr=ae->codeadr;
			ae->idx++;
		}
	} else {
		rasm_printf(ae,"[%s] Error line %d - ORG code location[,output location]\n",GetCurrentFile(ae),ae->wl[ae->idx].l);
		MaxError(ae);
		return;
	}
	
	/* check current ORG request */
	for (i=0;i<ae->io;i++) {
		if (ae->orgzone[i].memstart!=ae->orgzone[i].memend) {
			if (ae->orgzone[i].ibank==ae->activebank) {
				if (ae->outputadr<ae->orgzone[i].memend && ae->outputadr>=ae->orgzone[i].memstart) {
					if (ae->orgzone[i].protect) {
						rasm_printf(ae,"[%s] Error line %d - ORG located a PROTECTED section [#%04X-#%04X-B%d] file [%s] line %d\n",GetCurrentFile(ae),ae->wl[ae->idx].l,ae->orgzone[i].memstart,ae->orgzone[i].memend,ae->orgzone[i].ibank<32?ae->orgzone[i].ibank:0,ae->filename[ae->orgzone[i].ifile],ae->orgzone[i].iline);
					} else {
						rasm_printf(ae,"[%s] Error line %d - ORG (output at #%04X) located in a previous ORG section [#%04X-#%04X-B%d] file [%s] line %d\n",GetCurrentFile(ae),ae->wl[ae->idx].l,ae->outputadr,ae->orgzone[i].memstart,ae->orgzone[i].memend,ae->orgzone[i].ibank<32?ae->orgzone[i].ibank:0,ae->filename[ae->orgzone[i].ifile],ae->orgzone[i].iline);
					}
					MaxError(ae);
				}
			}
		}
	}
	
	OverWriteCheck(ae);
	
	/* if there was a crunch block before, now closed */
	if (ae->lz>=0) {
		ae->lz=-1;
	}	
	orgzone.memstart=ae->outputadr;
	orgzone.ibank=ae->activebank;
	orgzone.ifile=ae->wl[ae->idx].ifile;
	orgzone.iline=ae->wl[ae->idx].l;
	ObjectArrayAddDynamicValueConcat((void**)&ae->orgzone,&ae->io,&ae->mo,&orgzone,sizeof(orgzone));
}

void __HEXBIN(struct s_assenv *ae) {
	int hbinidx;
	unsigned int offset=0,size=0,idx;
	
	if (!ae->wl[ae->idx].t) {
		ExpressionFastTranslate(ae,&ae->wl[ae->idx+1].w,1);
		hbinidx=RoundComputeExpressionCore(ae,ae->wl[ae->idx+1].w,ae->codeadr,0);
		if (!ae->wl[ae->idx+1].t) {
			ExpressionFastTranslate(ae,&ae->wl[ae->idx+2].w,1);
			offset=RoundComputeExpressionCore(ae,ae->wl[ae->idx+2].w,ae->codeadr,0);
			if (!ae->wl[ae->idx+2].t) {
				ExpressionFastTranslate(ae,&ae->wl[ae->idx+3].w,1);
				size=RoundComputeExpressionCore(ae,ae->wl[ae->idx+3].w,ae->codeadr,0);
				if (size<1 || size>65536) {
					rasm_printf(ae,"[%s] Error line %d - INCBIN invalid size\n",GetCurrentFile(ae),ae->wl[ae->idx].l);
					MaxError(ae);
				}
				if (!ae->wl[ae->idx+3].t) {
					ExpressionFastTranslate(ae,&ae->wl[ae->idx+4].w,1);
					offset+=65536*RoundComputeExpressionCore(ae,ae->wl[ae->idx+4].w,ae->codeadr,0);
					ae->idx+=4;
				} else {
					ae->idx+=3;
				}
			} else {
				ae->idx+=2;
			}
		} else {
			ae->idx++;
		}

		if (hbinidx<ae->ih && hbinidx>=0) {
			if (!size) {
				if (!offset) {
					size=ae->hexbin[hbinidx].datalen;
				} else {
					size=ae->hexbin[hbinidx].datalen-offset;
				}
			}			
			if (size>ae->hexbin[hbinidx].datalen) {
				rasm_printf(ae,"[%s] Error line %d - INCBIN size is greater than filesize\n",GetCurrentFile(ae),ae->wl[ae->idx].l);
				MaxError(ae);
			} else {
				if (size+offset>ae->hexbin[hbinidx].datalen) {
					rasm_printf(ae,"[%s] Error line %d - INCBIN size+offset is greater than filesize\n",GetCurrentFile(ae),ae->wl[ae->idx].l);
					MaxError(ae);
				} else {
					for (idx=offset;idx<size+offset;idx++) {
						___output(ae,ae->hexbin[hbinidx].data[idx]);
					}
				}
			}
		} else {
			rasm_printf(ae,"[%s] INTERNAL error line %d - HEXBIN refer to unknown structure\n",GetCurrentFile(ae),ae->wl[ae->idx].l);
			MaxError(ae);
		}
	} else {
		rasm_printf(ae,"[%s] INTERNAL error line %d - HEXBIN need one HEX parameter\n",GetCurrentFile(ae),ae->wl[ae->idx].l);
		exit(2);
	}
}

void __SAVE(struct s_assenv *ae) {
	struct s_save cursave={0};
	unsigned int offset=0,size=0;
	int ko=1;
	
	if (!ae->wl[ae->idx].t) {
		/* nom de fichier entre quotes */
		if (!StringIsQuote(ae->wl[ae->idx+1].w)) {
			rasm_printf(ae,"[%s] Error line %d - SAVE invalid filename quote\n",GetCurrentFile(ae),ae->wl[ae->idx].l);
			MaxError(ae);
			ko=0;
		} else {
			if (!ae->wl[ae->idx+1].t) {
				if (!ae->wl[ae->idx+2].t && ae->wl[ae->idx+3].t==1) {
					cursave.ibank=ae->activebank;
					cursave.ioffset=ae->idx+2;
					cursave.isize=ae->idx+3;
					cursave.iw=ae->idx+1;
					ObjectArrayAddDynamicValueConcat((void**)&ae->save,&ae->nbsave,&ae->maxsave,&cursave,sizeof(cursave));
					ko=0;
				}
			}
		}
	}
	if (ko) {
		rasm_printf(ae,"[%s] Error line %d - Use SAVE 'filename',offset,size\n",GetCurrentFile(ae),ae->wl[ae->idx].l);
		MaxError(ae);
	}

	while (!ae->wl[ae->idx].t) ae->idx++;
}


void __MODULE(struct s_assenv *ae) {
	if (!ae->wl[ae->idx].t && ae->wl[ae->idx+1].t==1) {
		if (StringIsQuote(ae->wl[ae->idx+1].w)) {
			ae->module=MemMalloc(strlen(ae->wl[ae->idx+1].w));
			/* duplicate and remove quotes */
			strcpy(ae->module,ae->wl[ae->idx+1].w+1);
			ae->module[strlen(ae->module)-1]=0;
		} else {
			rasm_printf(ae,"[%s] Error line %d - MODULE directive need one text parameter\n",GetCurrentFile(ae),ae->wl[ae->idx].l);
			MaxError(ae);
		}
		ae->idx++;
	} else {
		if (ae->module) MemFree(ae->module);
		ae->module=NULL;
	}
}


struct s_asm_keyword instruction[]={
{"LD",0,_LD},
{"DEC",0,_DEC},
{"INC",0,_INC},
{"ADD",0,_ADD},
{"SUB",0,_SUB},
{"OR",0,_OR},
{"AND",0,_AND},
{"XOR",0,_XOR},
{"POP",0,_POP},
{"PUSH",0,_PUSH},
{"DJNZ",0,_DJNZ},
{"JR",0,_JR},
{"JP",0,_JP},
{"CALL",0,_CALL},
{"RET",0,_RET},
{"EX",0,_EX},
{"ADC",0,_ADC},
{"SBC",0,_SBC},
{"EXX",0,_EXX},
{"CP",0,_CP},
{"BIT",0,_BIT},
{"RES",0,_RES},
{"SET",0,_SET},
{"IN",0,_IN},
{"OUT",0,_OUT},
{"RLC",0,_RLC},
{"RRC",0,_RRC},
{"RL",0,_RL},
{"RR",0,_RR},
{"SLA",0,_SLA},
{"SRA",0,_SRA},
{"SLL",0,_SLL},
{"SRL",0,_SRL},
{"RST",0,_RST},
{"HALT",0,_HALT},
{"DI",0,_DI},
{"EI",0,_EI},
{"NOP",0,_NOP},
{"DEFB",0,_DEFB},
{"DB",0,_DEFB},
{"DEFW",0,_DEFW},
{"DW",0,_DEFW},
{"DEFS",0,_DEFS},
{"DS",0,_DEFS},
{"STR",0,_STR},
{"LDI",0,_LDI},
{"LDIR",0,_LDIR},
{"OUTI",0,_OUTI},
{"INI",0,_INI},
{"RLCA",0,_RLCA},
{"RRCA",0,_RRCA},
{"NEG",0,_NEG},
{"RLA",0,_RLA},
{"RRA",0,_RRA},
{"RLD",0,_RLD},
{"RRD",0,_RRD},
{"DAA",0,_DAA},
{"CPL",0,_CPL},
{"SCF",0,_SCF},
{"LDD",0,_LDD},
{"LDDR",0,_LDDR},
{"CCF",0,_CCF},
{"OUTD",0,_OUTD},
{"IND",0,_IND},
{"RETI",0,_RETI},
{"RETN",0,_RETN},
{"IM",0,_IM},
{"DEFI",0,_DEFI},
{"CPD",0,_CPD},
{"CPI",0,_CPI},
{"CPDR",0,_CPDR},
{"CPIR",0,_CPIR},
{"OTDR",0,_OTDR},
{"OTIR",0,_OTIR},
{"INDR",0,_INDR},
{"INIR",0,_INIR},
{"REPEAT",0,__REPEAT},
{"REND",0,__REND},
{"UNTIL",0,__UNTIL},
{"ORG",0,__ORG},
{"PROTECT",0,__PROTECT},
{"WHILE",0,__WHILE},
{"WEND",0,__WEND},
{"HEXBIN",0,__HEXBIN},
{"ALIGN",0,__ALIGN},
{"ELSEIF",0,__ELSEIF},
{"ELSE",0,__ELSE},
{"IF",0,__IF},
{"ENDIF",0,__ENDIF},
{"IFNOT",0,__IFNOT},
{"CASE",0,__CASE},
{"BREAK",0,__BREAK},
{"DEFAULT",0,__DEFAULT},
{"SWITCH",0,__SWITCH},
{"ENDSWITCH",0,__ENDSWITCH},
{"WRITE",0,__WRITE},
{"MACRO",0,__MACRO},
{"LET",0,__LET},
{"ASSERT",0,__ASSERT},
{"CHARSET",0,__CHARSET},
{"RUN",0,__RUN},
{"SAVE",0,__SAVE},
{"BRK",0,__BRK},
{"NOLIST",0,__NOLIST},
{"LIST",0,__LIST},
{"STOP",0,__STOP},
{"PRINT",0,__PRINT},
{"BANK",0,__BANK},
{"NAMEBANK",0,__NameBANK},
{"LIMIT",0,__LIMIT},
{"LZX7",0,__LZX7},
{"LZ4",0,__LZ4},
{"LZ48",0,__LZ48},
{"LZ49",0,__LZ49},
{"LZCLOSE",0,__LZCLOSE},
{"BUILDCPR",0,__BUILDCPR},
{"AMSDOS",0,__AMSDOS},
{"",0,NULL}
};

int Assemble(struct s_assenv *ae, unsigned char **dataout, int *lenout)
{
	#undef FUNC
	#define FUNC "Assemble"

	char AmsdosHeader[128]={0};
	struct s_expression curexp={0};
	struct s_wordlist *wordlist;
	struct s_expr_dico curdico={0};
	struct s_label *curlabel;
	int icrc,curcrc,i,j;
	unsigned char *lzdata=NULL;
	int lzlen,lzshift,input_size;
	size_t slzlen;
	unsigned char *input_data;
	struct s_orgzone orgzone={0};
	int iorgzone,ibank,offset,endoffset;
	int il,maxrom;
	char *TMP_filename=NULL;
	int minmem=65536,maxmem=0,lzmove;
	char symbol_line[1024];
	int ifast,executed;
	/* debug */
	int curii,inhibe;
	int ok;

	rasm_printf(ae,"Assembling\n");

	wordlist=ae->wl;
	ae->wl=wordlist;
	/* start outside crunched section */
	ae->lz=-1;
	
	/* default orgzone */
	orgzone.ibank=32;
	ObjectArrayAddDynamicValueConcat((void**)&ae->orgzone,&ae->io,&ae->mo,&orgzone,sizeof(orgzone));

	/* init des automates */
	InitAutomate(ae->AutomateValidLabel,AutomateValidLabelDefinition);
	InitAutomate(ae->AutomateValidLabelFirst,AutomateValidLabelFirstDefinition);

	InitAutomate(ae->AutomateExpressionValidChar,AutomateExpressionValidCharDefinition);
	ae->AutomateExpressionDecision['<']='<';
	ae->AutomateExpressionDecision['>']='>';
	ae->AutomateExpressionDecision['=']='=';
	ae->AutomateExpressionDecision['!']='!';
	ae->AutomateExpressionDecision[0]='E';
	/* gestion d'alias */
	ae->AutomateExpressionDecision['~']='~';
	

	/* variables par dÃ©faut */
	ExpressionSetDicoVar(ae,"PI",3.1415926545);
	ExpressionSetDicoVar(ae,"ASIC_PRI",0x6800);
	ExpressionSetDicoVar(ae,"ASIC_SPLIT",0x6801);
	ExpressionSetDicoVar(ae,"ASIC_SSA",0x6802);
	ExpressionSetDicoVar(ae,"ASIC_SSR",0x6804);
	
	/* pour simplifier les tests... */
	ObjectArrayAddDynamicValueConcat((void **)&ae->expression,&ae->ie,&ae->me,&curexp,sizeof(curexp));
	
	/* calcul des CRC pour les mots clefs et directives */
	for (icrc=0;instruction[icrc].mnemo[0];icrc++) instruction[icrc].crc=GetCRC(instruction[icrc].mnemo);
	for (icrc=0;math_keyword[icrc].mnemo[0];icrc++) math_keyword[icrc].crc=GetCRC(math_keyword[icrc].mnemo);

	/* Execution des mots clefs */
	ae->idx=1;
	while (wordlist[ae->idx].t!=2) {
		curcrc=GetCRC(wordlist[ae->idx].w);
		if (ae->verbose&4) {
			int iiii=0;
			rasm_printf(ae,"%d [%s] L%d [%s]",ae->idx,ae->filename[wordlist[ae->idx].ifile],wordlist[ae->idx].l,wordlist[ae->idx].w);
			while (!wordlist[ae->idx+iiii++].t) rasm_printf(ae," [%s]",wordlist[ae->idx+iiii].w);
			
			for (iiii=0;iiii<ae->imacropos;iiii++) {
				rasm_printf(ae,"M[%d] s=%d e=%d ",iiii,ae->macropos[iiii].start,ae->macropos[iiii].end);
			}
			
			rasm_printf(ae,"\n");
		}
		/* assemblage conditionnel */
		if (ae->ii || ae->isw) {
			/* inhibition des if/endif */
			for (inhibe=curii=0;curii<ae->ii;curii++) {
				if (!ae->ifthen[curii] || ae->ifthen[curii]==-1) {
					inhibe=1;
					break;
				}
			}
			/* inhibition des switch/case */
			if (!inhibe) {
				for (curii=0;curii<ae->isw;curii++) {
					if (!ae->switchcase[curii].execute) {
						inhibe=1;
						break;
					}
				}
			}
			/* en inhibition on cherche uniquement un prochain IF/IFNOT/ELSE/ELSEIF/ENDIF ou SWITCH/CASE/DEFAULT/ENDSWITCH */
			if (inhibe) {
				/* sorted by probability */
				if (curcrc==CRC_ELSEIF && strcmp(wordlist[ae->idx].w,"ELSEIF")==0) {
					__ELSEIF(ae);
				} else if (curcrc==CRC_CASE && strcmp(wordlist[ae->idx].w,"CASE")==0) {
					__CASE(ae);
				} else if (curcrc==CRC_BREAK && strcmp(wordlist[ae->idx].w,"BREAK")==0) {
					__BREAK(ae);
				} else if (curcrc==CRC_IF && strcmp(wordlist[ae->idx].w,"IF")==0) {
					__IF(ae);
				} else if (curcrc==CRC_SWITCH && strcmp(wordlist[ae->idx].w,"SWITCH")==0) {
					__SWITCH(ae);
				} else if (curcrc==CRC_IFNOT && strcmp(wordlist[ae->idx].w,"IFNOT")==0) {
					__IFNOT(ae);
				} else if (curcrc==CRC_ELSE && strcmp(wordlist[ae->idx].w,"ELSE")==0) {
					__ELSE(ae);
				} else if (curcrc==CRC_ENDIF && strcmp(wordlist[ae->idx].w,"ENDIF")==0) {
					__ENDIF(ae);
				} else if (curcrc==CRC_ENDSWITCH && strcmp(wordlist[ae->idx].w,"ENDSWITCH")==0) {
					__ENDSWITCH(ae);
				} else if (curcrc==CRC_DEFAULT && strcmp(wordlist[ae->idx].w,"DEFAULT")==0) {
					__DEFAULT(ae);
				}
				while (wordlist[ae->idx].t==0) ae->idx++;
				ae->idx++;
				continue;
			}
		}
		if (ae->imacropos) {
			/* on se trouve encore dans la macro? */
			if (ae->idx>=ae->macropos[0].end) {
				/* on est sorti de tous les blocs de repetition? */
				if (!ae->ir && !ae->iw) {
					ae->imacropos=0;
				}
			}
		}

		executed=0;
		if ((ifast=ae->fastmatch[(int)wordlist[ae->idx].w[0]])!=-1) {
			while (instruction[ifast].mnemo[0]==wordlist[ae->idx].w[0]) {
				if (instruction[ifast].crc==curcrc && strcmp(instruction[ifast].mnemo,wordlist[ae->idx].w)==0) {
					instruction[ifast].makemnemo(ae);
					executed=1;
					break;
				}
				ifast++;
			}
		}
		if (!executed) {
			/* est-ce une macro? */
			if ((ifast=SearchMacro(ae,curcrc,wordlist[ae->idx].w))>=0) {
				wordlist=__MACRO_EXECUTE(ae,ifast);
				continue;
			}
		}
		if (!executed) {
			/* ni instruction ni directive, un seul mot clef, c'est un label ou une affectation! */
			if (wordlist[ae->idx].e) {
				ExpressionFastTranslate(ae,&wordlist[ae->idx].w,0);
				RoundComputeExpression(ae,wordlist[ae->idx].w,ae->codeadr,0,0); // quel intérêt du Round?
			} else {
				PushLabel(ae);
			}
		} else {
			while (!wordlist[ae->idx].t) ae->idx++;
		}
		ae->idx++;
	}
	if (ae->verbose&4) {
		rasm_printf(ae,"%d [%s] L%d [%s] fin de la liste de mots\n",ae->idx,ae->filename[wordlist[ae->idx].ifile],wordlist[ae->idx].l,wordlist[ae->idx].w);
	}

	/* fin de l'assemblage, on clot le ORG en cours */
	if (ae->io) {
		ae->orgzone[ae->io-1].memend=ae->outputadr;
	}
	OverWriteCheck(ae);
	/* fin de l'assemblage, on clot le crunch en cours (si besoin) */
	__internal_UpdateLZBlockIfAny(ae);
	
	for (i=0;i<ae->ir;i++) {
		rasm_printf(ae,"[%s] L%d REPEAT was not closed\n",ae->filename[wordlist[ae->repeat[i].start].ifile],wordlist[ae->repeat[i].start].l);
		MaxError(ae);
	}
	for (i=0;i<ae->iw;i++) {
		rasm_printf(ae,"[%s] L%d WHILE was not closed\n",ae->filename[wordlist[ae->whilewend[i].start].ifile],wordlist[ae->whilewend[i].start].l);
		MaxError(ae);
	}

	
	/*
		crunch des sections LZ
		update des labels anterieurs si dans le meme orgzone
	*/
	for (i=0;i<ae->ilz;i++) {
		/* compute labels and expression inside crunched blocks */
		PopAllExpression(ae,i);
		
		ae->curlz=i;
		iorgzone=ae->lzsection[i].iorgzone;
		ibank=ae->lzsection[i].ibank;
#if 0
		/*
			si la section LZ n'est pas la premiere du ORG courant, on doit forcer le calcul
			de ses expressions (qui ont ete relogees apres le traitement du LZ precedent).
		*/
		if (ae->lzsection[i].lz==2) {
			printf("forcer calcul des expressions du LZ\n");
			/* @TODO remplacer par un fast index dans le ORG! */
			for (il=0;il<ae->ie && (ae->expression[il].iorgzone!=iorgzone || ae->expression[il].ibank!=ibank || ae->expression[il].wptr<ae->lzsection[i].memstart || ae->expression[il].wptr>ae->lzsection[i].memend);il++);
			while (il<ae->ie && ae->expression[il].iorgzone==iorgzone && ae->expression[il].ibank==ibank && ae->expression[il].wptr<=ae->lzsection[i].memend) {
				printf("expr[%s](%d) set to 1\n",ae->expression[il].reference?ae->expression[il].reference:wordlist[ae->expression[il].iw].w,ae->expression[il].crunch_block);
				ae->expression[il].crunch_block=1;
				il++;
			}
			/* reloger les labels aussi (idem fast index dans le ORG) */
			/* on se positionne sur tous les pointeurs de la zone */
			for (il=0;il<ae->il && (ae->label[il].iorgzone!=iorgzone || ae->label[il].ibank!=ibank);il++);
			while (il<ae->il && ae->label[il].iorgzone==iorgzone && ae->label[il].ibank==ibank) {
				/* on devrait shifter des labels deja utilises mais tant pis c'est plus simple a traiter de cette facon */
				printf("reset label [%s] to 1\n",ae->label[il].iw!=-1?wordlist[ae->label[il].iw].w:ae->label[il].name);
				ae->label[il].lz=1;
				il++;
			}
			
			/* on peut calculer les expr a nouveau @TODO faire un acces rapide */
			PopAllExpression(ae,1);
		}
#endif
		input_data=&ae->mem[ae->lzsection[i].ibank][ae->lzsection[i].memstart];
		input_size=ae->lzsection[i].memend-ae->lzsection[i].memstart;

		switch (ae->lzsection[i].lzversion) {
			case 7:
				#ifndef NO_3RD_PARTIES
				lzdata=ZX7_compress(optimize(input_data, input_size), input_data, input_size, &slzlen);
				lzlen=slzlen;
				#endif
				break;
			case 4:
				#ifndef NO_3RD_PARTIES
				lzdata=LZ4_crunch(input_data,input_size,&lzlen);
				#endif
				break;
			case 8:
				#ifndef NO_3RD_PARTIES
				lzdata=Exomizer_crunch(input_data,input_size,&lzlen);
				#endif
				break;
			case 48:
				lzdata=LZ48_crunch(input_data,input_size,&lzlen);
				break;
			case 49:
				lzdata=LZ49_crunch(input_data,input_size,&lzlen);
				break;
			default:
				rasm_printf(ae,"Internal error - unknown crunch method %d\n",ae->lzsection[i].lzversion);
				exit(-12);
		}
//rasm_printf(ae,"lzsection[%d] type=%d start=%04X end=%04X crunched size=%d\n",i,ae->lzsection[i].lzversion,ae->lzsection[i].memstart,ae->lzsection[i].memend,lzlen);

		lzshift=lzlen-(ae->lzsection[i].memend-ae->lzsection[i].memstart);
		if (lzshift>0) {
			MemMove(ae->mem[ae->lzsection[i].ibank]+ae->lzsection[i].memend+lzshift,ae->mem[ae->lzsection[i].ibank]+ae->lzsection[i].memend,65536-ae->lzsection[i].memend-lzshift);
		} else if (lzshift<0) {
			lzmove=ae->orgzone[iorgzone].memend-ae->lzsection[i].memend;
			if (lzmove) {
				MemMove(ae->mem[ae->lzsection[i].ibank]+ae->lzsection[i].memend+lzshift,ae->mem[ae->lzsection[i].ibank]+ae->lzsection[i].memend,lzmove);
			}
		}
		memcpy(ae->mem[ae->lzsection[i].ibank]+ae->lzsection[i].memstart,lzdata,lzlen);
		MemFree(lzdata);
		/* reloger les pointeurs */
		
		/* on se positionne sur les pointeurs situes apres la crunched zone (dans le meme org) */
		il=ae->lzsection[i].ilabel;
		while (il<ae->il && ae->label[il].iorgzone==iorgzone && ae->label[il].ibank==ibank) {
			curlabel=SearchLabel(ae,ae->label[il].iw!=-1?wordlist[ae->label[il].iw].w:ae->label[il].name,ae->label[il].crc);
			/* CANNOT be NULL */
			curlabel->ptr+=lzshift;
//printf("label [%s] shifte de %d valeur #%04X -> #%04X\n",curlabel->iw!=-1?wordlist[curlabel->iw].w:curlabel->name,lzshift,curlabel->ptr-lzshift,curlabel->ptr);
			il++;
		}
		/* reloger les expressions situess apres la crunched zone (dans le meme org) */
		il=ae->lzsection[i].iexpr;
		while (il<ae->ie && ae->expression[il].iorgzone==iorgzone && ae->expression[il].ibank==ibank) {
			ae->expression[il].wptr+=lzshift;
			ae->expression[il].ptr+=lzshift;
//printf("expression [%s] shiftee ptr=#%04X wptr=#%04X\n",ae->expression[il].reference?ae->expression[il].reference:wordlist[ae->expression[il].iw].w,ae->expression[il].ptr,ae->expression[il].wptr);
			il++;
		}
		/* reloger les autres sections LZ!!! */
//rasm_printf(ae,"lzsection[%d] O%d B%d\n",i,ae->lzsection[i].iorgzone,ae->lzsection[i].ibank);
		il=i+1;
		while (il<ae->ilz && ae->lzsection[il].iorgzone==iorgzone && ae->lzsection[il].ibank==ibank) {
			//rasm_printf(ae,"reloger lzsection[%d] O%d B%d\n",il,ae->lzsection[il].iorgzone,ae->lzsection[il].ibank);
			ae->lzsection[il].memstart+=lzshift;
			ae->lzsection[il].memend+=lzshift;
			il++;
		}
		
		/* reloge le ORG courant au crunch */
		ae->orgzone[iorgzone].memend+=lzshift;
	}
	if (ae->ilz) {
		/* compute expression placed after the last crunched block */
		PopAllExpression(ae,ae->ilz);
	}
	/* compute expression outside crunched blocks */
	PopAllExpression(ae,-1);
	

/******* ecriture des donnees assemblees *********/

	TMP_filename=MemMalloc(strlen(ae->outputfilename)+5);

	if (!ae->nberr) {
		
		/* enregistrement des fichiers programmes par la commande SAVE */
		PopAllSave(ae);
		
		if (ae->nbsave==0 || (ae->cprmode && ae->forcecpr)) {
			if (ae->cprmode) {
				char ChunkName[5];
				int ChunkSize;
				int do_it=1;
				
				if (ae->cartridge_name) {
					sprintf(TMP_filename,"%s",ae->cartridge_name);
				} else {
					sprintf(TMP_filename,"%s.cpr",ae->outputfilename);
				}
				FileRemoveIfExists(TMP_filename);
				
				rasm_printf(ae,"Write cartridge file %s\n",TMP_filename);
				for (i=maxrom=0;i<ae->io;i++) {
					if (ae->orgzone[i].ibank<32 && ae->orgzone[i].ibank>maxrom) maxrom=ae->orgzone[i].ibank;
				}
				/* construction du CPR */
				/* header blablabla */
				strcpy(ChunkName,"RIFF");
				FileWriteBinary(TMP_filename,ChunkName,4);
				ChunkSize=(maxrom+1)*(16384+8)+4;
				FileWriteBinary(TMP_filename,(char*)&ChunkSize,4);
				sprintf(ChunkName,"AMS!");
				FileWriteBinary(TMP_filename,ChunkName,4);
				
//				for (j=0;j<ae->io;j++) {
//printf("ORG[%03d]=B%02d/#%04X/#%04X\n",j,ae->orgzone[j].ibank,ae->orgzone[j].memstart,ae->orgzone[j].memend);
//				}
				for (i=0;i<=maxrom;i++) {
					offset=65536;
					endoffset=0;
					for (j=0;j<ae->io;j++) {
						/* bank data may start anywhere (typically #0000 or #C000) */
						if (ae->orgzone[j].ibank==i && ae->orgzone[j].memstart!=ae->orgzone[j].memend) {
							if (ae->orgzone[j].memstart<offset) offset=ae->orgzone[j].memstart;
							if (ae->orgzone[j].memend>endoffset) endoffset=ae->orgzone[j].memend;
						}
					}
					if (ae->verbose & 8) {
						if (endoffset>offset) {
							int lm=0;
							if (ae->iwnamebank[i]>0) {
								lm=strlen(ae->wl[ae->iwnamebank[i]].w)-2;
							}
							rasm_printf(ae,"WriteCPR bank %2d of %5d byte%s start at #%04X",i,endoffset-offset,endoffset-offset>1?"s":" ",offset);
							if (endoffset-offset>16384) {
								rasm_printf(ae,"\nROM is too big!!!\n");
								FileWriteBinaryClose(TMP_filename);
								FileRemoveIfExists(TMP_filename);
								exit(ABORT_ERROR);
							}
							if (lm) {
								rasm_printf(ae," (%-*.*s)\n",lm,lm,ae->wl[ae->iwnamebank[i]].w+1);
							} else {
								rasm_printf(ae,"\n");
							}
						} else {
							rasm_printf(ae,"WriteCPR bank %2d (empty)\n",i);
						}
					}
					ChunkSize=16384;
					sprintf(ChunkName,"cb%02d",i);
					FileWriteBinary(TMP_filename,ChunkName,4);
					FileWriteBinary(TMP_filename,(char*)&ChunkSize,4);
					if (offset>0xC000) {
						unsigned char filler[16384]={0};
						ChunkSize=65536-offset;
						if (ChunkSize) FileWriteBinary(TMP_filename,(char*)ae->mem[i]+offset,ChunkSize);
						/* ADD zeros until the end of the bank */
						FileWriteBinary(TMP_filename,(char*)filler,16384-ChunkSize);
					} else {
						FileWriteBinary(TMP_filename,(char*)ae->mem[i]+offset,ChunkSize);
					}
				}
				FileWriteBinaryClose(TMP_filename);
				rasm_printf(ae,"Total %d bank%s (%dK)\n",maxrom+1,maxrom+1>1?"s":"",(maxrom+1)*16);
			} else {
				if (ae->binary_name) {
					sprintf(TMP_filename,"%s",ae->binary_name);
				} else {
					sprintf(TMP_filename,"%s.bin",ae->outputfilename);
				}
				FileRemoveIfExists(TMP_filename);
				
				for (i=0;i<ae->io;i++) {
					/* uniquement si le ORG a ete suivi d'ecriture */
					if (ae->orgzone[i].memstart!=ae->orgzone[i].memend) {
						if (ae->orgzone[i].memstart<minmem) minmem=ae->orgzone[i].memstart;
						if (ae->orgzone[i].memend>maxmem) maxmem=ae->orgzone[i].memend;
					}
				}
				if (maxmem-minmem<=0) {
					rasm_printf(ae,"Warning: Not a single byte to output\n");
				} else {
					if (!ae->flux) {
						rasm_printf(ae,"Write binary file %s (%d byte%s)\n",TMP_filename,maxmem-minmem,maxmem-minmem>1?"s":"");
						if (ae->amsdos) {
							int checksum;
							/***  cpcwiki			
							Byte 00: User number
							Byte 01 to 08: filename
							Byte 09 bis 11: Extension
							Byte 18: type-byte
							Byte 21 and 22: loading address
							Byte 24 and 25: file length
							Byte 26 and 27: execution address for machine code programs
							Byte 64 and 65: (file length)
							Byte 67 and 68: checksum for byte 00 to byte 66
							To calculate the checksum, just add byte 00 to byte 66 to each other.
							*/				
							rasm_printf(ae,"Amsdos header @TOCHECK");
							memset(AmsdosHeader,0,sizeof(AmsdosHeader));
							AmsdosHeader[0]=0;
							strcpy(AmsdosHeader+1,"CODEDATABIN");
							AmsdosHeader[18]=2; /* 0 basic 1 basic protege 2 binaire */
							
							AmsdosHeader[21]=minmem&0xFF;
							AmsdosHeader[22]=minmem>>8;
							AmsdosHeader[24]=(maxmem-minmem)&0xFF;
							AmsdosHeader[25]=(maxmem-minmem)>>8;
							AmsdosHeader[26]=minmem&0xFF;
							AmsdosHeader[27]=minmem>>8;
							AmsdosHeader[64]=(maxmem-minmem)&0xFF;
							AmsdosHeader[65]=(maxmem-minmem)>>8;
							
							for (i=checksum=0;i<=66;i++) {
								checksum+=AmsdosHeader[i];
							}
							AmsdosHeader[67]=checksum&0xFF;
							AmsdosHeader[68]=checksum>>8;
							FileWriteBinary(TMP_filename,AmsdosHeader,128);
						}
						if (maxmem-minmem>0) {
							FileWriteBinary(TMP_filename,(char*)ae->mem[32]+minmem,maxmem-minmem);
							FileWriteBinaryClose(TMP_filename);
						} else {
							if (ae->amsdos) {
								FileWriteBinaryClose(TMP_filename);
							}
						}
					} else {
						int curbank;
						curbank=ae->orgzone[0].ibank;
						*dataout=MemMalloc(maxmem-minmem+1);
						memcpy(*dataout,ae->mem[curbank]+minmem,maxmem-minmem);
						*lenout=maxmem-minmem;
					}
				}
			}
		}
		/* export symboles */
		if (ae->symbol_name) {
			sprintf(TMP_filename,"%s",ae->symbol_name);
		} else {
			sprintf(TMP_filename,"%s.sym",ae->outputfilename);
		}		
		FileRemoveIfExists(TMP_filename);
		if (ae->verbose & 7 && ae->export_sym) {
			rasm_printf(ae,"Export symbols in [%s]\n",TMP_filename);
		}
		switch (ae->export_sym) {
			case 3:
				/* winape */
				for (i=0;i<ae->il;i++) {
					if (!ae->label[i].name) {
						sprintf(symbol_line,"%s #%04X\n",ae->wl[ae->label[i].iw].w,ae->label[i].ptr);
						FileWriteLine(TMP_filename,symbol_line);
					} else {
						if (ae->export_local) {
							sprintf(symbol_line,"%s #%04X\n",ae->label[i].name,ae->label[i].ptr);
							FileWriteLine(TMP_filename,symbol_line);
						}
					}
				}
				if (ae->export_var) {
					/* var are part of fast tree search structure */
					ExportDicoTree(ae,TMP_filename,"%s #%04X\n");
				}
				if (ae->export_equ) {
					for (i=0;i<ae->ialias;i++) {
						sprintf(symbol_line,"%s #%04X\n",ae->alias[i].alias,RoundComputeExpression(ae,ae->alias[i].translation,0,0,0));
						FileWriteLine(TMP_filename,symbol_line);
					}
				}
				FileWriteLineClose(TMP_filename);
				break;
			case 2:
				/* pasmo */
				for (i=0;i<ae->il;i++) {
					if (!ae->label[i].name) {
						sprintf(symbol_line,"%s EQU 0%04XH\n",ae->wl[ae->label[i].iw].w,ae->label[i].ptr);
						FileWriteLine(TMP_filename,symbol_line);
					} else {
						if (ae->export_local) {
							sprintf(symbol_line,"%s EQU 0%04XH\n",ae->label[i].name,ae->label[i].ptr);
							FileWriteLine(TMP_filename,symbol_line);
						}
					}
				}
				if (ae->export_var) {
					/* var are part of fast tree search structure */
					ExportDicoTree(ae,TMP_filename,"%s EQU 0%04XH\n");
				}
				if (ae->export_equ) {
					for (i=0;i<ae->ialias;i++) {
						sprintf(symbol_line,"%s EQU 0%04XH\n",ae->alias[i].alias,RoundComputeExpression(ae,ae->alias[i].translation,0,0,0));
						FileWriteLine(TMP_filename,symbol_line);
					}
				}
				FileWriteLineClose(TMP_filename);
				break;
			case 1:
				/* Rasm */
				for (i=0;i<ae->il;i++) {
					if (!ae->label[i].name) {
						sprintf(symbol_line,"%s #%X B%d\n",ae->wl[ae->label[i].iw].w,ae->label[i].ptr,ae->label[i].ibank>31?0:ae->label[i].ibank);
						FileWriteLine(TMP_filename,symbol_line);
					} else {
						if (ae->export_local) {
							sprintf(symbol_line,"%s #%X B%d\n",ae->label[i].name,ae->label[i].ptr,ae->label[i].ibank>31?0:ae->label[i].ibank);
							FileWriteLine(TMP_filename,symbol_line);
						}
					}
				}
				if (ae->export_var) {
					/* var are part of fast tree search structure */
					ExportDicoTree(ae,TMP_filename,"%s #%X B0\n");
				}
				if (ae->export_equ) {
					for (i=0;i<ae->ialias;i++) {
						sprintf(symbol_line,"%s #%X B0\n",ae->alias[i].alias,RoundComputeExpression(ae,ae->alias[i].translation,0,0,0));
						FileWriteLine(TMP_filename,symbol_line);
					}
				}
				FileWriteLineClose(TMP_filename);
				break;
			case 0:
			default:break;	
		}
	} else {
		rasm_printf(ae,"%d error%s\n",ae->nberr,ae->nberr>1?"s":"");
		minmem=65536;
		maxmem=0;
		for (i=0;i<ae->io;i++) {
			/* uniquement si le ORG a ete suivi d'ecriture */
			if (ae->orgzone[i].memstart!=ae->orgzone[i].memend) {
				if (ae->orgzone[i].memstart<minmem) minmem=ae->orgzone[i].memstart;
				if (ae->orgzone[i].memend>maxmem) maxmem=ae->orgzone[i].memend;
			}
		}
	}

	if (ae->verbose&(4+2) && maxmem-minmem>0) {
		printf("%04X - ",minmem);
		i=minmem&0xF;
		while (i) {
			printf("   ");
			i--;
		}

		/* dump hexa du code assemble, pour tests */
		il=0;
		for (i=minmem;i<maxmem;i++) {
			rasm_printf(ae,"%02X ",ae->mem[32][i]);
			if (!((i+1)%16)) {
				rasm_printf(ae," | ");
				for (j=i-15;j<=i;j++) {
					if (ae->mem[32][j]>=32 && ae->mem[32][j]<128) {
						rasm_printf(ae,"%c",ae->mem[32][j]);
					} else {
						rasm_printf(ae,".");
					}
				}
				rasm_printf(ae,"\n%04X - ",i+1);
				il=i+1;
			}
		}
		i--;
		if (i && ((i+1)%16)) {
			rasm_printf(ae,"   ");
			j=i+1;
			while (((j+1)%16)) {
				rasm_printf(ae,"   ");
				j++;
			}
			rasm_printf(ae," | ");
			for (j=il;j<i+1;j++) {
				if (ae->mem[32][j]>=32 && ae->mem[32][j]<128) {
					rasm_printf(ae,"%c",ae->mem[32][j]);
				} else {
					rasm_printf(ae,".");
				}
			}
			rasm_printf(ae,"\n");
		}
		rasm_printf(ae,"\n");
	}

	if (ae->verbose & 7) {
		rasm_printf(ae,"------ statistics ------------------\n");
		rasm_printf(ae,"%d file%s\n",ae->ifile,ae->ifile>1?"s":"");
		rasm_printf(ae,"%d binary include%s\n",ae->ih,ae->ih>1?"s":"");
		rasm_printf(ae,"%d word%s\n",ae->nbword-1,ae->nbword>2?"s":"");
		rasm_printf(ae,"%d label%s\n",ae->il,ae->il>1?"s":"");
		rasm_printf(ae,"%d var%s\n",ae->idic,ae->idic>1?"s":"");
		rasm_printf(ae,"%d expression%s\n",ae->ie,ae->ie>1?"s":"");
		rasm_printf(ae,"%d macro%s\n",ae->imacro,ae->imacro>1?"s":"");
		rasm_printf(ae,"%d alias%s\n",ae->ialias,ae->ialias>1?"s":"");
		rasm_printf(ae,"%d ORG zone%s\n",ae->io-1,ae->io>2?"s":"");
		rasm_printf(ae,"%d virtual space%s\n",ae->nbbank,ae->nbbank>1?"s":"");
	}

	/* liberation memoire */
	for (i=0;i<ae->nbbank;i++) {
		MemFree(ae->mem[i]);
	}
	MemFree(ae->mem);
	
	/* liberation buffer operation */
	ComputeExpressionCore(NULL,NULL,0,0);
	ExpressionFastTranslate(NULL,NULL,0);
	/* free labels, expression, orgzone, repeat, ... */
	if (ae->mo) MemFree(ae->orgzone);
	if (ae->me) {
		for (i=0;i<ae->ie;i++) if (ae->expression[i].reference) MemFree(ae->expression[i].reference);
		MemFree(ae->expression);
	}
	if (ae->mh) {
		for (i=0;i<ae->ih;i++) {
			MemFree(ae->hexbin[i].data);
		}
		MemFree(ae->hexbin);
	}
	for (i=0;i<ae->il;i++) {
		if (ae->label[i].name && ae->label[i].iw==-1) MemFree(ae->label[i].name);
	}
	if (ae->ml) MemFree(ae->label);
	if (ae->mr) MemFree(ae->repeat);
	if (ae->mi) MemFree(ae->ifthen);
	if (ae->msw) MemFree(ae->switchcase);
	if (ae->mw) MemFree(ae->whilewend);
	for (i=0;i<ae->idic;i++) {
		MemFree(ae->dico[i].name);
	}
	if (ae->mdic) MemFree(ae->dico);
	if (ae->mlz) MemFree(ae->lzsection);

	for (i=0;i<ae->ifile;i++) {
		MemFree(ae->filename[i]);
	}
	MemFree(ae->filename);

	for (i=0;i<ae->imacro;i++) {
		if (ae->macro[i].maxword) MemFree(ae->macro[i].wc);
		for (j=0;j<ae->macro[i].nbparam;j++) MemFree(ae->macro[i].param[j]);
		if (ae->macro[i].nbparam) MemFree(ae->macro[i].param);
	}
	if (ae->mmacro) MemFree(ae->macro);

	for (i=0;i<ae->ialias;i++) {
		MemFree(ae->alias[i].alias);
		MemFree(ae->alias[i].translation);
	}
	if (ae->malias) MemFree(ae->alias);

	for (i=0;wordlist[i].t!=2;i++) {
		MemFree(wordlist[i].w);
	}
	MemFree(wordlist);

	if (ae->nberr) {
		ok=-1;
		if (ae->flux) {
			MemFree(*dataout);
			*dataout=NULL;
			*lenout=0;
		}
	} else {
		ok=0;
	}
	MemFree(TMP_filename);
	MemFree(ae->outputfilename);
	FreeLabelTree(ae);
	FreeDicoTree(ae);
	if (ae->mmacropos) MemFree(ae->macropos);
	MemFree(ae);
	return ok;
}


void StateMachineRemoveComz(char *Abuf) {
	#undef FUNC
	#define FUNC "StateMachineRemoveComz"

	int i=0,q=0;
	while (Abuf[i]) {
		if (Abuf[i]==';' && !q) {
			while (Abuf[i] && Abuf[i]!=0x0D && Abuf[i]!=0x0A) Abuf[i++]=':';
			i--;
		} else if (Abuf[i]=='"' && !q) {
			q='"';
		} else if (Abuf[i]=='\'' && !q && i>2 && !(toupper(Abuf[i-2])=='A' && toupper(Abuf[i-1])=='F')) {
			q='\'';
		} else if (Abuf[i]==q) {
			q=0;
		}
		i++;
	}
	if (i && Abuf[i-1]==0x0A) Abuf[i-1]=':';
	//strcat(Abuf,":");
}

void PreProcessingSplitListing(struct s_listing **listing, int *il, int *ml, int idx, int end, int start)
{
	#undef FUNC
	#define FUNC "PreProcessingSplitListing"
	
	struct s_listing curlisting={0};
	
	ObjectArrayAddDynamicValueConcat((void**)listing,il,ml,&curlisting,sizeof(curlisting));
	MemMove(&((*listing)[idx+2]),&((*listing)[idx+1]),(*il-idx-2)*sizeof(struct s_listing));
	(*listing)[idx+1].ifile=(*listing)[idx].ifile;
	(*listing)[idx+1].iline=(*listing)[idx].iline;
	if ((*listing)[idx].listing[start]) {
		(*listing)[idx+1].listing=TxtStrDup((*listing)[idx].listing+start);
	} else {
		(*listing)[idx+1].listing=TxtStrDup(";");
	}
	strcpy((*listing)[idx].listing+end,":");
}

void PreProcessingInsertListing(struct s_listing **reflisting, int *il, int *ml, int idx, char **zelines, int ifile)
{
	#undef FUNC
	#define FUNC "PreProcessingSplitListing"
	
	struct s_listing *listing;
	int nbinsert,li,bil;
	for (li=nbinsert=0;zelines[li];li++) nbinsert++;
	bil=*il;
	if (*il+nbinsert>=*ml) {
		*il=*ml=*il+nbinsert;
		*reflisting=MemRealloc(*reflisting,sizeof(struct s_listing)*(*ml));
	} else {
		*il=*il+nbinsert;
	}
	listing=*reflisting;
	MemMove(&listing[idx+1+nbinsert],&listing[idx+1],(bil-idx-1)*sizeof(struct s_listing));
	
	for (li=0;zelines[li];li++) {
		listing[idx+1+li].ifile=ifile;
		listing[idx+1+li].iline=li+1;
		listing[idx+1+li].listing=zelines[li];
	}
}

int cmpkeyword(const void * a, const void * b)
{
	struct s_asm_keyword *sa,*sb;
	sa=(struct s_asm_keyword *)a;
	sb=(struct s_asm_keyword *)b;
	return strcmp(sa->mnemo,sb->mnemo);
}

struct s_assenv *PreProcessing(char *filename, int flux, const char *datain, int datalen, int verbose, char *outputfilename, char *labelfilename)
{
	#undef FUNC
	#define FUNC "PreProcessing"

	#define CharWord "@ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789.=_($)][+-*/^%#|&'\"\\§}{"

	struct s_assenv *ae=NULL;
	struct s_wordlist curw={0};
	struct s_wordlist *wordlist=NULL;
	int nbword=0,maxword=0;
	char **zelines=NULL;

	char *filename_toread;
	
	struct s_macro_fast *MacroFast=NULL;
	int idxmacrofast=0,maxmacrofast=0;
	
	struct s_listing *listing=NULL;
	struct s_listing curlisting;
	int ilisting=0,maxlisting=0;
	
	char **listing_include=NULL;
	int i,l=0,idx=0,c=0,li,le;
	char Automate[256]={0};
	struct s_hexbin curhexbin;
	char *newlistingline=NULL;
	unsigned char *newdata;
	struct s_label curlabel={0};
	char *labelsep1;
	char **labelines=NULL;
	/* state machine buffer */
	unsigned char *mem=NULL;
	char *w=NULL,*wtmp=NULL;
	int lw=0,mw=256;
	char *bval=NULL;
	int ival=0,sval=256;
	char *qval=NULL;
	int iqval=0,sqval=256;
	struct s_repeat_index *TABrindex=NULL;
	struct s_repeat_index *TABwindex=NULL;
	struct s_repeat_index rindex={0};
	struct s_repeat_index windex={0};
	int nri=0,mri=0,ri=0;
	int nwi=0,mwi=0,wi=0;
	/* state machine trigger */
	int waiting_quote=0,lquote;
	int macro_trigger=0;
	int escape_code=0;
	int quote_type=0;
	int incbin=0,include=0,crunch=0;
	int rewrite=0;
	int nbinstruction;
	int ifast,texpr;
	int ispace=0;

	windex.cl=-1;
	windex.cidx=-1;
	rindex.cl=-1;
	rindex.cidx=-1;

	ae=MemMalloc(sizeof(struct s_assenv));
	memset(ae,0,sizeof(struct s_assenv));
	ae->flux=flux;

	
	/*
		winape		sprintf(symbol_line,"%s #%4X\n",ae->label[i].name,ae->label[i].ptr);
		pasmo		sprintf(symbol_line,"%s EQU 0%4XH\n",ae->label[i].name,ae->label[i].ptr);
		rasm 		sprintf(symbol_line,"%s #%X B%d\n",ae->wl[ae->label[i].iw].w,ae->label[i].ptr,ae->label[i].ibank>31?0:ae->label[i].ibank);
	*/
	
	if (labelfilename) {
		rasm_printf(ae,"Label import from [%s]\n",labelfilename);
		labelines=FileReadLines(labelfilename);
		i=0;
		while (labelines[i]) {
			if ((labelsep1=strstr(labelines[i]," EQU 0"))!=NULL) {
				/* pasmo */
				*labelsep1=0;
				curlabel.name=labelines[i];
				curlabel.iw=-1;
				curlabel.crc=GetCRC(curlabel.name);
				curlabel.ptr=strtol(labelsep1+6,NULL,16);
				ObjectArrayAddDynamicValueConcat((void **)&ae->label,&ae->il,&ae->ml,&curlabel,sizeof(curlabel));
			} else if ((labelsep1=strstr(labelines[i]," "))!=NULL) {
				/* winape / rasm */
				if (*(labelsep1+1)=='#') {
					*labelsep1=0;
					curlabel.name=labelines[i];
					curlabel.iw=-1;
					curlabel.crc=GetCRC(curlabel.name);
					curlabel.ptr=strtol(labelsep1+2,NULL,16);
					ObjectArrayAddDynamicValueConcat((void **)&ae->label,&ae->il,&ae->ml,&curlabel,sizeof(curlabel));
				}
			}
			i++;
		}
		MemFree(labelines);
	}
	/* 32 CPR default roms + one workspace */
	for (i=0;i<33;i++) {
		mem=MemMalloc(65536);
		memset(mem,0,65536);
		ObjectArrayAddDynamicValueConcat((void**)&ae->mem,&ae->nbbank,&ae->maxbank,&mem,sizeof(mem));
	}
	ae->activebank=32;
	ae->maxptr=65536;
	ae->verbose=verbose;
	for (i=0;i<256;i++) { ae->charset[i]=(unsigned char)i; }
	if (outputfilename) {
		ae->outputfilename=TxtStrDup(outputfilename);
	} else {
		ae->outputfilename=TxtStrDup("rasmoutput");
	}

	rasm_printf(ae,"Pre-processing [%s]\n",filename);
	for (nbinstruction=0;instruction[nbinstruction].mnemo[0];nbinstruction++);
	qsort(instruction,nbinstruction,sizeof(struct s_asm_keyword),cmpkeyword);
	for (i=0;i<256;i++) { ae->fastmatch[i]=-1; }
	for (i=0;i<nbinstruction;i++) { if (ae->fastmatch[(int)instruction[i].mnemo[0]]==-1) ae->fastmatch[(int)instruction[i].mnemo[0]]=i; } 
	for (i=0;CharWord[i];i++) {Automate[((int)CharWord[i])&0xFF]=1;}
	 /* separators */
	Automate[' ']=2;
	Automate[',']=2;
	Automate['\t']=2;
	/* end of line */
	Automate[':']=3; /* les 0x0A et 0x0D seront deja  remplaces en ':' */
	/* expression */
	Automate['=']=4; /* on stocke l'emplacement de l'egalite */
	Automate['<']=4; /* ou des operateurs */
	Automate['>']=4; /* d'evaluation */
	Automate['!']=4;

	StateMachineResizeBuffer(&w,256,&mw);
	StateMachineResizeBuffer(&bval,256,&sval);
	StateMachineResizeBuffer(&qval,256,&sqval);
	w[0]=0;
	bval[0]=0;
	qval[0]=0;
	
	if (!ae->flux) {
		zelines=FileReadLines(filename);
		FieldArrayAddDynamicValueConcat(&ae->filename,&ae->ifile,&ae->maxfile,filename);
	} else {
		zelines=MemMalloc(2*sizeof(char *));
		zelines[0]=MemMalloc(datalen+3);
		zelines[1]=NULL;
		/* copie des données */
		memcpy(zelines[0],datain,datalen);
		/* et on ajoute un petit zéro à la fin! */
		zelines[0][datalen]=0;
		/* en mode flux on prend le repertoire courant en reference */
		FieldArrayAddDynamicValueConcat(&ae->filename,&ae->ifile,&ae->maxfile,CURRENT_DIR);
	}	

	for (i=0;zelines[i];i++) {
		curlisting.ifile=0;
		curlisting.iline=i+1;
		curlisting.listing=zelines[i];
		ObjectArrayAddDynamicValueConcat((void**)&listing,&ilisting,&maxlisting,&curlisting,sizeof(curlisting));
	}
	MemFree(zelines);

	/* on s'assure que la derniere instruction est prise en compte a peu de frais */
	if (ilisting) {
		datalen=strlen(listing[ilisting-1].listing);
		listing[ilisting-1].listing=MemRealloc(listing[ilisting-1].listing,datalen+2);
		listing[ilisting-1].listing[datalen]=':';
		listing[ilisting-1].listing[datalen+1]=0;
	}
	/* virer les commentaires, scanner un ; */
	for (l=0;l<ilisting;l++) StateMachineRemoveComz(listing[l].listing);

	l=0;
	while (l<ilisting) {
		c=listing[l].listing[idx++];
		if (!c) {
			l++;
			idx=0;
			continue;
		} else if (c=='\\' && !waiting_quote) {
			idx++;
			continue;
		} else if (c==0x0D || c==0x0A) {
			listing[l].listing[idx-1]=':';
			c=':';
		} else if (c=='\'' && idx>2 && strncmp(&listing[l].listing[idx-3],"AF'",3)==0) {
			/* rien */
		} else if (c=='"' || c=='\'') {
			if (!quote_type) {
				quote_type=c;
				lquote=l;
			} else {
				if (c==quote_type) {
					quote_type=0;
				}
			}
		}

		if (waiting_quote) {
			/* expecting quote and nothing else */
			switch (waiting_quote) {
				case 1:
					if (c==quote_type) waiting_quote=2;
					break;
				case 2:
					if (!quote_type) {
						waiting_quote=3;
						qval[iqval]=0;
					} else {
						qval[iqval++]=c;
						StateMachineResizeBuffer(&qval,iqval,&sqval);
						qval[iqval]=0;
					}
			}
			if (waiting_quote==3) {
				if (incbin) {
					/* qval contient le nom du fichier a lire */
					filename_toread=MergePath(ae,ae->filename[listing[l].ifile],qval);
					
					if (FileExists(filename_toread)) {
						/* lecture */
						curhexbin.datalen=FileGetSize(filename_toread);
						curhexbin.data=MemMalloc(curhexbin.datalen*1.3+10);
						if (ae->verbose & 7) {
							switch (crunch) {
								case 0:rasm_printf(ae,"incbin [%s] size=%d\n",filename_toread,curhexbin.datalen);break;
								case 4:rasm_printf(ae,"inclz4 [%s] size=%d\n",filename_toread,curhexbin.datalen);break;
								case 7:rasm_printf(ae,"incsx7 [%s] size=%d\n",filename_toread,curhexbin.datalen);break;
								case 8:rasm_printf(ae,"incexo [%s] size=%d\n",filename_toread,curhexbin.datalen);break;
								case 88:rasm_printf(ae,"incexb [%s] size=%d\n",filename_toread,curhexbin.datalen);break;
								case 48:rasm_printf(ae,"incl48 [%s] size=%d\n",filename_toread,curhexbin.datalen);break;
								case 49:rasm_printf(ae,"incl49 [%s] size=%d\n",filename_toread,curhexbin.datalen);break;
								default:rasm_printf(ae,"invalid crunch state!\n");exit(-42);
							}
						}
						if (FileReadBinary(filename_toread,(char*)curhexbin.data,curhexbin.datalen)!=curhexbin.datalen) {
							rasm_printf(ae,"read error on %s",filename_toread);
							exit(2);
						}
						FileReadBinaryClose(filename_toread);
						switch (crunch) {
							#ifndef NO_3RD_PARTIES
							case 4:
								newdata=LZ4_crunch(curhexbin.data,curhexbin.datalen,&curhexbin.datalen);
								MemFree(curhexbin.data);
								curhexbin.data=newdata;
								if (ae->verbose & 7) {
									rasm_printf(ae,"crunched into %d byte(s)\n",curhexbin.datalen);
								}
								break;
							case 7:
								{
								size_t slzlen;
								newdata=ZX7_compress(optimize(curhexbin.data, curhexbin.datalen), curhexbin.data, curhexbin.datalen, &slzlen);
								curhexbin.datalen=slzlen;
								MemFree(curhexbin.data);
								curhexbin.data=newdata;
								if (ae->verbose & 7) {
									rasm_printf(ae,"crunched into %d byte(s)\n",curhexbin.datalen);
								}
								}
								break;
							case 8:
								newdata=Exomizer_crunch(curhexbin.data,curhexbin.datalen,&curhexbin.datalen);
								MemFree(curhexbin.data);
								curhexbin.data=newdata;
								if (ae->verbose & 7) {
									rasm_printf(ae,"crunched into %d byte(s)\n",curhexbin.datalen);
								}
								break;
							#endif
							case 48:
								newdata=LZ48_crunch(curhexbin.data,curhexbin.datalen,&curhexbin.datalen);
								MemFree(curhexbin.data);
								curhexbin.data=newdata;
								if (ae->verbose & 7) {
									rasm_printf(ae,"crunched into %d byte(s)\n",curhexbin.datalen);
								}
								break;
							case 49:
								newdata=LZ49_crunch(curhexbin.data,curhexbin.datalen,&curhexbin.datalen);
								MemFree(curhexbin.data);
								curhexbin.data=newdata;
								if (ae->verbose & 7) {
									rasm_printf(ae,"crunched into %d byte(s)\n",curhexbin.datalen);
								}
								break;
							default:break;
						}
						ObjectArrayAddDynamicValueConcat((void**)&ae->hexbin,&ae->ih,&ae->mh,&curhexbin,sizeof(curhexbin));
						
						/* insertion */
						le=strlen(listing[l].listing);

						newlistingline=MemMalloc(le+32);
						memcpy(newlistingline,listing[l].listing,rewrite);
						rewrite+=sprintf(newlistingline+rewrite,"HEXBIN #%X",ae->ih-1);
						strcat(newlistingline+rewrite,listing[l].listing+idx);
						idx=rewrite;
						MemFree(listing[l].listing);
						listing[l].listing=newlistingline;
					} else {
						rasm_printf(ae,"[%s] Error line %d / file [%s] not found\n",ae->filename[listing[lquote].ifile],listing[l].iline,filename_toread);
						MaxError(ae);
					}
					incbin=0;
				} else if (include) {
					/* qval contient le nom du fichier a lire */
					filename_toread=TxtStrDup(MergePath(ae,ae->filename[listing[l].ifile],qval));
					if (FileExists(filename_toread)) {
						if (ae->verbose & 7) rasm_printf(ae,"include [%s]\n",filename_toread);
						
						/* lecture */
						listing_include=FileReadLines(filename_toread);
						FieldArrayAddDynamicValueConcat(&ae->filename,&ae->ifile,&ae->maxfile,filename_toread);
						MemFree(filename_toread);
						/* virer les commentaires */
						for (li=0;listing_include[li];li++)	{
							StateMachineRemoveComz(listing_include[li]);
						}
						/* split de la ligne en cours + suppression de l'instruction include */
						PreProcessingSplitListing(&listing,&ilisting,&maxlisting,l,rewrite,idx);
						/* insertion des nouvelles lignes + reference fichier + numeros de ligne */
						PreProcessingInsertListing(&listing,&ilisting,&maxlisting,l,listing_include,ae->ifile-1);
						
						MemFree(listing_include); /* free le tableau mais pas les lignes */
						listing_include=NULL;
						idx=0; /* on reste sur la meme ligne mais on se prepare a relire du caractere 0! */
					} else {
						rasm_printf(ae,"[%s] Error line %d / file [%s] not found\n",ae->filename[listing[lquote].ifile],listing[l].iline,filename_toread);
						MaxError(ae);
					}
					include=0;
				}
				waiting_quote=0;
				qval[0]=0;
				iqval=0;
			}
		} else {
			/* classic behaviour */

			/* upper case or avoid quote analyse */
			if (!quote_type) {
				if (c>='a' && c<='z') listing[l].listing[idx-1]=c=c-'a'+'A';
			}
			/* looking for include/incbin */
			if (((c>='A' && c<='Z') || (c>='0' && c<='9') || c=='@' || c=='_')&& !quote_type) {
				bval[ival++]=c;
				StateMachineResizeBuffer(&bval,ival,&sval);
				bval[ival]=0;
			} else {
				if (strcmp(bval,"INCLUDE")==0) {
					include=1;
					waiting_quote=1;
					rewrite=idx-7-1;
					/* quote right after keyword */
					if (c==quote_type) {
						waiting_quote=2;
					}
				} else if (strcmp(bval,"READ")==0) {
					include=1;
					waiting_quote=1;
					rewrite=idx-4-1;
					/* quote right after keyword */
					if (c==quote_type) {
						waiting_quote=2;
					}
				} else if (strcmp(bval,"INCLZ4")==0) {
					incbin=1;
					crunch=4;
					waiting_quote=1;
					rewrite=idx-6-1;
					/* quote right after keyword */
					if (c==quote_type) {
						waiting_quote=2;
					}
				} else if (strcmp(bval,"INCEXB")==0) {
					incbin=1;
					crunch=88;
					waiting_quote=1;
					rewrite=idx-6-1;
					/* quote right after keyword */
					if (c==quote_type) {
						waiting_quote=2;
					}
				} else if (strcmp(bval,"INCEXO")==0) {
					incbin=1;
					crunch=8;
					waiting_quote=1;
					rewrite=idx-6-1;
					/* quote right after keyword */
					if (c==quote_type) {
						waiting_quote=2;
					}
				} else if (strcmp(bval,"INCZX7")==0) {
					incbin=1;
					crunch=7;
					waiting_quote=1;
					rewrite=idx-6-1;
					/* quote right after keyword */
					if (c==quote_type) {
						waiting_quote=2;
					}
				} else if (strcmp(bval,"INCL48")==0) {
					incbin=1;
					crunch=48;
					waiting_quote=1;
					rewrite=idx-6-1;
					/* quote right after keyword */
					if (c==quote_type) {
						waiting_quote=2;
					}
				} else if (strcmp(bval,"INCL49")==0) {
					incbin=1;
					crunch=49;
					waiting_quote=1;
					rewrite=idx-6-1;
					/* quote right after keyword */
					if (c==quote_type) {
						waiting_quote=2;
					}
				} else if (strcmp(bval,"INCBIN")==0) {
					incbin=1;
					crunch=0;
					waiting_quote=1;
					rewrite=idx-6-1;
					/* quote right after keyword */
					if (c==quote_type) {
						waiting_quote=2;
					}
				} else if (strcmp(bval,"WHILE")==0) {
					/* remplir la structure repeat_index */
					windex.ol=listing[l].iline;
					windex.oidx=idx;
					windex.ifile=ae->ifile-1;
					ObjectArrayAddDynamicValueConcat((void**)&TABwindex,&nwi,&mwi,&windex,sizeof(windex));
				} else if (strcmp(bval,"REPEAT")==0) {
					/* remplir la structure repeat_index */
					rindex.ol=listing[l].iline;
					rindex.oidx=idx;
					rindex.ifile=ae->ifile-1;
					ObjectArrayAddDynamicValueConcat((void**)&TABrindex,&nri,&mri,&rindex,sizeof(rindex));
				} else if (strcmp(bval,"WEND")==0) {
					/* retrouver la structure repeat_index correspondant a l'ouverture */
					for (wi=nwi-1;wi>=0;wi--) {
						if (TABwindex[wi].cl==-1) {
							TABwindex[wi].cl=c;
							TABwindex[wi].cidx=idx;
							break;
						}
					}
					if (wi==-1) {
						rasm_printf(ae,"[%s] Error line %d / WEND refers to unknown WHILE\n",ae->filename[listing[l].ifile],listing[l].iline);
						exit(1);
					}
				} else if (strcmp(bval,"REND")==0 || strcmp(bval,"UNTIL")==0) {
					/* retrouver la structure repeat_index correspondant a l'ouverture */
					for (ri=nri-1;ri>=0;ri--) {
						if (TABrindex[ri].cl==-1) {
							TABrindex[ri].cl=c;
							TABrindex[ri].cidx=idx;
							break;
						}
					}
					if (ri==-1) {
						rasm_printf(ae,"[%s] Error line %d / %s refers to unknown REPEAT\n",ae->filename[listing[l].ifile],listing[l].iline,bval);
						exit(1);
					}
						
				}
				bval[0]=0;
				ival=0;
			}
		}
	}
	if (quote_type) {
		rasm_printf(ae,"[%s] Error line %d quote opened was not closed\n",ae->filename[listing[lquote].ifile],listing[lquote].iline);
		exit(1);
	}

	/* repeat expansion check */
	for (ri=0;ri<nri;ri++) {
		if (TABrindex[ri].cl==-1) {
			rasm_printf(ae,"[%s] Error line %d Repeat has no end\n",ae->filename[TABrindex[ri].ifile],TABrindex[ri].ol+1);
			MaxError(ae);
		}
	}

	/* creer une liste de mots */

	curw.w=TxtStrDup("BEGIN");
	curw.l=0;
	curw.ifile=0;
	curw.t=1;
	curw.e=0;
	ObjectArrayAddDynamicValueConcat((void**)&wordlist,&nbword,&maxword,&curw,sizeof(curw));

	/* pour les calculs d'adresses avec IX et IY on enregistre deux labels bidons du meme nom */
	curw.e=2;
	curw.w=TxtStrDup("IX~0");
	ObjectArrayAddDynamicValueConcat((void**)&wordlist,&nbword,&maxword,&curw,sizeof(curw));
	curw.w=TxtStrDup("IY~0");
	ObjectArrayAddDynamicValueConcat((void**)&wordlist,&nbword,&maxword,&curw,sizeof(curw));
	curw.e=0;
	
#if 0
	l=0;
	while (l<ilisting) {
		rasm_printf(ae,"listing[%d]\n%s\n",l,listing[l].listing);
		l++;
	}
#endif	

	texpr=quote_type=0;
	l=lw=idx=0;
	ispace=0;
	w[0]=0;
	while (l<ilisting) {
		c=listing[l].listing[idx++];
		if (!c) {
			idx=0;
			l++;
			continue;
		}

		if (!quote_type) {
			switch (Automate[((int)c)&0xFF]) {
				case 0:
					rasm_printf(ae,"[%s] L%d - Invalid char '%c' (%d) char %d\n",ae->filename[listing[l].ifile],listing[l].iline,c,c,idx);
					MaxError(ae);
					break;
				case 1:
					if (c=='\'' && idx>2 && strncmp(&listing[l].listing[idx-3],"AF'",3)==0) {
						w[lw++]=c;
						StateMachineResizeBuffer(&w,lw,&mw);
						w[lw]=0;
						break;
					} else if (c=='\'' || c=='"') {
						quote_type=c;
						/* debut d'une quote, on finalise le mot -> POURQUOI DONC? */
						//idx--;

						/* on finalise le mot si on est en début d'une nouvelle instruction ET que c'est un SAVE */
						if (strcmp(w,"SAVE")==0) {
							idx--;
						} else {
							w[lw++]=c;
							StateMachineResizeBuffer(&w,lw,&mw);
							w[lw]=0;
							break;
						}
					} else {
						if (c!=' ' && c!='\t') {
							w[lw++]=c;
							StateMachineResizeBuffer(&w,lw,&mw);
							w[lw]=0;
						} else {
							/* Winape/Maxam operator compatibility on expressions */
							if (texpr) {
								if (strcmp(w+ispace,"AND")==0) {
									w[ispace]='&';
									lw=ispace+1;
								} else if (strcmp(w+ispace,"OR")==0) {
									w[ispace]='|';
									lw=ispace+1;
								} else if (strcmp(w+ispace,"MOD")==0) {
									w[ispace]='§';
									lw=ispace+1;
								} else if (strcmp(w+ispace,"XOR")==0) {
									w[ispace]='^';
									lw=ispace+1;
								}
							}
							ispace=lw;
						}
						break;
					}
				case 2:
					/* separator */
					if (lw) {
						w[lw]=0;
						if (texpr && !wordlist[nbword-1].t && wordlist[nbword-1].e) {
							/* pour compatibilite winape avec AND,OR,XOR */
							if (strcmp(w,"AND")==0) {
								wtmp=TxtStrDup("&");
							} else if (strcmp(w,"OR")==0) {
								wtmp=TxtStrDup("|");
							} else if (strcmp(w,"XOR")==0) {
								wtmp=TxtStrDup("^");
							} else {
								wtmp=TxtStrDup(w);
							}
							nbword--;
							lw=0;
							for (li=0;wordlist[nbword].w[li];li++) {
								w[lw++]=wordlist[nbword].w[li];
								StateMachineResizeBuffer(&w,lw,&mw);
							}
							w[lw]=0;
							MemFree(wordlist[nbword].w);
							
							for (li=0;wtmp[li];li++) {
								w[lw++]=wtmp[li];
								StateMachineResizeBuffer(&w,lw,&mw);
							}
							w[lw]=0;
							MemFree(wtmp);
							/* et on modifie l'automate pour la suite! */
							Automate[' ']=1;
							Automate['\t']=1;
							ispace=lw;
						} else if (strcmp(w,"EQU")==0) {
							/* il y avait un mot avant alors on va reorganiser la ligne */
							nbword--;
							lw=0;
							for (li=0;wordlist[nbword].w[li];li++) {
								w[lw++]=wordlist[nbword].w[li];
								StateMachineResizeBuffer(&w,lw,&mw);
							}
							MemFree(wordlist[nbword].w);
							curw.e=lw+1;
							/* on ajoute l'egalite d'alias*/
							w[lw++]='~';
							StateMachineResizeBuffer(&w,lw,&mw);
							w[lw]=0;
							Automate[' ']=1;
							Automate['\t']=1;
							ispace=lw;
							texpr=1;
						} else {
							curw.w=TxtStrDup(w);
							curw.l=listing[l].iline;
							curw.ifile=listing[l].ifile;
							curw.t=0;
							ObjectArrayAddDynamicValueConcat((void**)&wordlist,&nbword,&maxword,&curw,sizeof(curw));
							curw.e=0;
							lw=0;
							w[lw]=0;

							/* match keyword? then next spaces will be ignored*/
							if (macro_trigger) {
								struct s_macro_fast curmacrofast;
								Automate[' ']=1;
								Automate['\t']=1;
								ispace=0;
								texpr=1;
								macro_trigger=0;
								/* add macro name to instruction pool for preprocessor */
								curmacrofast.mnemo=curw.w;
								curmacrofast.crc=GetCRC(curw.w);
								ObjectArrayAddDynamicValueConcat((void**)&MacroFast,&idxmacrofast,&maxmacrofast,&curmacrofast,sizeof(struct s_macro_fast));	
							} else {
								int keymatched=0;
								if ((ifast=ae->fastmatch[(int)curw.w[0]])!=-1) {
									while (instruction[ifast].mnemo[0]==curw.w[0]) {
										if (strcmp(instruction[ifast].mnemo,curw.w)==0) {
											keymatched=1;														
											if (strcmp(curw.w,"MACRO")==0 || strcmp(curw.w,"WRITE")==0) {
												macro_trigger=1;
											} else {
												Automate[' ']=1;
												Automate['\t']=1;
												ispace=0;
												/* instruction en cours, le reste est a interpreter comme une expression */
												texpr=1;
											}
											break;
										}
										ifast++;
									}
								}
								if (!keymatched) {
									int macrocrc;
									macrocrc=GetCRC(curw.w);
									for (keymatched=0;keymatched<idxmacrofast;keymatched++) {
										if (MacroFast[keymatched].crc==macrocrc)
										if (strcmp(MacroFast[keymatched].mnemo,curw.w)==0) {
												Automate[' ']=1;
												Automate['\t']=1;
												ispace=0;
												/* macro en cours, le reste est a interpreter comme une expression */
												texpr=1;
												break;
										}
									}
								}
							}
						}
					} else {
//printf("mot vide\n");
					}
					break;
				case 3:
					/* fin de ligne, on remet l'automate comme il faut */
					Automate[' ']=2;
					Automate['\t']=2;
					ispace=0;
					texpr=0;
					/* si le mot lu a plus d'un caractère */
					if (lw) {
						if (texpr && nbword && wordlist[nbword-1].e) {
							/* cas particulier d'ecriture libre */
							nbword--;
							lw=0;
							for (li=0;wordlist[nbword].w[li];li++) {
								w[lw++]=wordlist[nbword].w[li];
								StateMachineResizeBuffer(&w,lw,&mw);
								w[lw]=0;
							}
							MemFree(wordlist[nbword].w);
							for (li=0;wtmp[li];li++) {
								w[lw++]=wtmp[li];
								StateMachineResizeBuffer(&w,lw,&mw);
								w[lw]=0;
							}
							MemFree(wtmp);
							
							curw.w=TxtStrDup(w);
							curw.l=listing[l].iline;
							curw.ifile=listing[l].ifile;
							curw.t=1;
							ObjectArrayAddDynamicValueConcat((void**)&wordlist,&nbword,&maxword,&curw,sizeof(curw));
							curw.e=0;
							lw=0;
							w[lw]=0;
						} else if (nbword && strcmp(w,"EQU")==0) {
							/* il y avait un mot avant alors on va reorganiser la ligne */
							nbword--;
							lw=0;
							for (li=0;wordlist[nbword].w[li];li++) {
								w[lw++]=wordlist[nbword].w[li];
								StateMachineResizeBuffer(&w,lw,&mw);
								w[lw]=0;
							}
							MemFree(wordlist[nbword].w);
							/* on ajoute l'egalite ou comparaison! */
							curw.e=lw+1;
							w[lw++]='=';
							StateMachineResizeBuffer(&w,lw,&mw);
							w[lw]=0;
							Automate[' ']=1;
							Automate['\t']=1;
						} else {
							curw.w=TxtStrDup(w);
							curw.l=listing[l].iline;
							curw.ifile=listing[l].ifile;
							curw.t=1;
							ObjectArrayAddDynamicValueConcat((void**)&wordlist,&nbword,&maxword,&curw,sizeof(curw));
							curw.e=0;
							lw=0;
							w[lw]=0;
						}
					} else {
						/* sinon c'est le précédent qui était terminateur d'instruction */
						wordlist[nbword-1].t=1;
						w[lw]=0;
					}
					break;
				case 4:
					/* expression/condition */
					texpr=1;
				    if (lw) {
						Automate[' ']=1;
						Automate['\t']=1;
						if (!curw.e) {
							curw.e=lw+1;
							w[lw++]=c;
							StateMachineResizeBuffer(&w,lw,&mw);
							w[lw]=0;
						} else {
							w[lw++]=c;
							StateMachineResizeBuffer(&w,lw,&mw);
							w[lw]=0;
							/* on autorise uniquement !=, ==, <=, >= et une seule fois */
							if (c!='=' || lw!=curw.e+1) {
								rasm_printf(ae,"[%s] L%d - too much equality symbol '='\n",ae->filename[listing[l].ifile],listing[l].iline);
								MaxError(ae);
							}
						}
					} else {
						if (!wordlist[nbword-1].t) {
							/* il y avait un mot avant alors on va reorganiser la ligne */
							nbword--;
							for (li=0;wordlist[nbword].w[li];li++) {
								w[lw++]=wordlist[nbword].w[li];
								StateMachineResizeBuffer(&w,lw,&mw);
								w[lw]=0;
							}
							MemFree(wordlist[nbword].w);
							/* on ajoute l'egalite ou comparaison! */
							curw.e=lw+1;
							w[lw++]=c;
							StateMachineResizeBuffer(&w,lw,&mw);
							w[lw]=0;
							/* et on modifie l'automate pour la suite! */
							Automate[' ']=1;
							Automate['\t']=1;
						} else {
							rasm_printf(ae,"[%s] L%d - cannot start expression with '=','!','<','>'\n",ae->filename[listing[l].ifile],listing[l].iline);
							MaxError(ae);
						}
					}
					break;
				default:
					rasm_printf(ae,"Internal error (Automate wrong value=%d)\n",Automate[c]);
					exit(-1);
			}
		} else {
			/* lecture inconditionnelle de la quote */
			w[lw++]=c;
			StateMachineResizeBuffer(&w,lw,&mw);
			w[lw]=0;
			if (!escape_code) {
				if (c=='\\') escape_code=1;
				if (lw>1 && c==quote_type) {
					quote_type=0;
				}
			} else {
				escape_code=0;
			}
		}
	}

	curw.w="END";
	curw.l=0;
	curw.t=2;
	curw.ifile=0;
	ObjectArrayAddDynamicValueConcat((void**)&wordlist,&nbword,&maxword,&curw,sizeof(curw));

	if (ae->verbose & 7) rasm_printf(ae,"wordlist contains %d element%s\n",nbword,nbword>1?"s":"");
	ae->nbword=nbword;
	
	if (ae->verbose==2) {
		for (l=0;l<nbword;l++) {
			rasm_printf(ae,"[%s]e=%d ",wordlist[l].w,wordlist[l].e);
			if (wordlist[l].t) rasm_printf(ae,"(%d)\n",wordlist[l].t);
		}
	}


	if (nri) MemFree(TABrindex);
	MemFree(bval);
	MemFree(qval);
	MemFree(w);

	for (l=0;l<ilisting;l++) {
			MemFree(listing[l].listing);
	}	
	MemFree(listing);

	/* liste de mots de type 0 ou 1, terminÃ©e par un mot vide de type 2 */
	ae->wl=wordlist;
	return ae;
}

void Rasm(char *filename, int export_sym, int verbose, char *outputfilename, float rough, int export_local, char *labelfilename, int export_var, int export_equ, char *symbol_name, char *binary_name, char *cartridge_name)
{
	#undef FUNC
	#define FUNC "Rasm"

	struct s_assenv *ae=NULL;

	/* read and preprocess source */
	ae=PreProcessing(filename,0,NULL,0,verbose,outputfilename,labelfilename);
	ae->export_local=export_local;
	ae->export_sym=export_sym;
	ae->export_var=export_var;
	ae->export_equ=export_equ;
	ae->rough=rough;
	ae->symbol_name=symbol_name;
	ae->binary_name=binary_name;
	ae->cartridge_name=cartridge_name;
	if (rough) ae->maxam=0; else ae->maxam=1;
	/* assemble */
	Assemble(ae,NULL,NULL);
}

/* fonction d'export */

int RasmAssemble(const char *datain, int lenin, unsigned char **dataout, int *lenout)
{
	struct s_assenv *ae=NULL;
	
	ae=PreProcessing(NULL,1,datain,lenin,0x0,NULL,NULL);
	return Assemble(ae,dataout,lenout);
}

#define AUTOTEST_MACRO "macro glop:@glop:ld hl,@next:djnz @glop:@next:mend:macro glop2:@glop:glop:ld hl,@next:djnz @glop:glop:@next:mend:cpti=0:repeat:glop:cpt=0:glop:repeat:glop2:repeat 1:@glop:dec a:ld hl,@next:glop2:glop2:jr nz,@glop:@next:rend:cpt=cpt+1:glop2:until cpt<3:cpti=cpti+1:glop2:until cpti<3"

#define AUTOTEST_OPCODES "nop::ld bc,#1234::ld (bc),a::inc bc:inc b:dec b:ld b,#12:rlca:ex af,af':add hl,bc:ld a,(bc):dec bc:inc c:dec c:ld c,#12:rrca::djnz $:ld de,#1234:ld (de),a:inc de:inc d:dec d:ld d,#12:rla:jr $:add hl,de:ld a,(de):dec de:inc e:dec e:ld e,#12:rra::jr nz,$:ld hl,#1234:ld (#1234),hl:inc hl:inc h:dec h:ld h,#12:daa:jr z,$:add hl,hl:ld hl,(#1234):dec hl:inc l:dec l:ld l,#12:cpl::jr nc,$:ld sp,#1234:ld (#1234),a:inc sp:inc (hl):dec (hl):ld (hl),#12:scf:jr c,$:add hl,sp:ld a,(#1234):dec sp:inc a:dec a:ld a,#12:ccf::ld b,b:ld b,c:ld b,d:ld b,e:ld b,h:ld b,l:ld b,(hl):ld b,a:ld c,b:ld c,c:ld c,d:ld c,e:ld c,h:ld c,l:ld c,(hl):ld c,a::ld d,b:ld d,c:ld d,d:ld d,e:ld d,h:ld d,l:ld d,(hl):ld d,a:ld e,b:ld e,c:ld e,d:ld e,e:ld e,h:ld e,l:ld e,(hl):ld e,a::ld h,b:ld h,c:ld h,d:ld h,e:ld h,h:ld h,l:ld h,(hl):ld h,a:ld l,b:ld l,c:ld l,d:ld l,e:ld l,h:ld l,l:ld l,(hl):ld l,a::ld (hl),b:ld (hl),c:ld (hl),d:ld (hl),e:ld (hl),h:ld (hl),l:halt:ld (hl),a:ld a,b:ld a,c:ld a,d:ld a,e:ld a,h:ld a,l:ld a,(hl):ld a,a::add b:add c:add d:add e:add h:add l:add (hl):add a:adc b:adc c:adc d:adc e:adc h:adc l:adc (hl):adc a::sub b:sub c:sub d:sub e:sub h:sub l:sub (hl):sub a:sbc b:sbc c:sbc d:sbc e:sbc h:sbc l:sbc (hl):sbc a::and b:and c:and d:and e:and h:and l:and (hl):and a:xor b:xor c:xor d:xor e:xor h:xor l:xor (hl):xor a::or b:or c:or d:or e:or h:or l:or (hl):or a:cp b:cp c:cp d:cp e:cp h:cp l:cp (hl):cp a::ret nz:pop bc:jp nz,#1234:jp #1234:call nz,#1234:push bc:add #12:rst 0:ret z:ret:jp z,#1234:nop:call z,#1234:call #1234:adc #12:rst 8::ret nc:pop de:jp nc,#1234:out (#12),a:call nc,#1234:push de:sub #12:rst #10:ret c:exx:jp c,#1234:in a,(#12):call c,#1234:nop:sbc #12:rst #18::ret po:pop hl:jp po,#1234:ex (sp),hl:call po,#1234:push hl:and #12:rst #20:ret pe:jp (hl):jp pe,#1234:ex de,hl:call pe,#1234:nop:xor #12:rst #28::ret p:pop af:jp p,#1234:di:call p,#1234:push af:or #12:rst #30:ret m:ld sp,hl:jp m,#1234:ei:call m,#1234:nop:cp #12:rst #38:in b,(c):out (c),b:sbc hl,bc:ld (#1234),bc:neg:retn:im 0:ld i,a:in c,(c):out (c),c:adc hl,bc:ld bc,(#1234):reti:ld r,a::in d,(c):out (c),d:sbc hl,de:ld (#1234),de:retn:im 1:ld a,i:in e,(c):out (c),e:adc hl,de:ld de,(#1234):im 2:ld a,r::in h,(c):out (c),h:sbc hl,hl:rrd:in l,(c):out (c),l:adc hl,hl:rld::in 0,(c):out (c),0:sbc hl,sp:ld (#1234),sp:in a,(c):out (c),a:adc hl,sp:ld sp,(#1234)::ldi:cpi:ini:outi:ldd:cpd:ind:outd::ldir:cpir:inir:otir:lddr:cpdr:indr:otdr::rlc b:rlc c:rlc d:rlc e:rlc h:rlc l:rlc (hl):rlc a:rrc b:rrc c:rrc d:rrc e:rrc h:rrc l:rrc (hl):rrc a::rl b:rl c:rl d:rl e:rl h:rl l:rl (hl):rl a:rr b:rr c:rr d:rr e:rr h:rr l:rr (hl):rr a:sla b:sla c:sla d:sla e:sla h:sla l:sla (hl):sla a:sra b:sra c:sra d:sra e:sra h:sra l:sra (hl):sra a::sll b:sll c:sll d:sll e:sll h:sll l:sll (hl):sll a:srl b:srl c:srl d:srl e:srl h:srl l:srl (hl):srl a::bit 0,b:bit 0,c:bit 0,d:bit 0,e:bit 0,h:bit 0,l:bit 0,(hl):bit 0,a::bit 1,b:bit 1,c:bit 1,d:bit 1,e:bit 1,h:bit 1,l:bit 1,(hl):bit 1,a::bit 2,b:bit 2,c:bit 2,d:bit 2,e:bit 2,h:bit 2,l:bit 2,(hl):bit 2,a::bit 3,b:bit 3,c:bit 3,d:bit 3,e:bit 3,h:bit 3,l:bit 3,(hl):bit 3,a::bit 4,b:bit 4,c:bit 4,d:bit 4,e:bit 4,h:bit 4,l:bit 4,(hl):bit 4,a::bit 5,b:bit 5,c:bit 5,d:bit 5,e:bit 5,h:bit 5,l:bit 5,(hl):bit 5,a::bit 6,b:bit 6,c:bit 6,d:bit 6,e:bit 6,h:bit 6,l:bit 6,(hl):bit 6,a::bit 7,b:bit 7,c:bit 7,d:bit 7,e:bit 7,h:bit 7,l:bit 7,(hl):bit 7,a::res 0,b:res 0,c:res 0,d:res 0,e:res 0,h:res 0,l:res 0,(hl):res 0,a::res 1,b:res 1,c:res 1,d:res 1,e:res 1,h:res 1,l:res 1,(hl):res 1,a::res 2,b:res 2,c:res 2,d:res 2,e:res 2,h:res 2,l:res 2,(hl):res 2,a::res 3,b:res 3,c:res 3,d:res 3,e:res 3,h:res 3,l:res 3,(hl):res 3,a::res 4,b:res 4,c:res 4,d:res 4,e:res 4,h:res 4,l:res 4,(hl):res 4,a::res 5,b:res 5,c:res 5,d:res 5,e:res 5,h:res 5,l:res 5,(hl):res 5,a::res 6,b:res 6,c:res 6,d:res 6,e:res 6,h:res 6,l:res 6,(hl):res 6,a::res 7,b:res 7,c:res 7,d:res 7,e:res 7,h:res 7,l:res 7,(hl):res 7,a::set 0,b:set 0,c:set 0,d:set 0,e:set 0,h:set 0,l:set 0,(hl):set 0,a::set 1,b:set 1,c:set 1,d:set 1,e:set 1,h:set 1,l:set 1,(hl):set 1,a::set 2,b:set 2,c:set 2,d:set 2,e:set 2,h:set 2,l:set 2,(hl):set 2,a::set 3,b:set 3,c:set 3,d:set 3,e:set 3,h:set 3,l:set 3,(hl):set 3,a::set 4,b:set 4,c:set 4,d:set 4,e:set 4,h:set 4,l:set 4,(hl):set 4,a::set 5,b:set 5,c:set 5,d:set 5,e:set 5,h:set 5,l:set 5,(hl):set 5,a::set 6,b:set 6,c:set 6,d:set 6,e:set 6,h:set 6,l:set 6,(hl):set 6,a::set 7,b:set 7,c:set 7,d:set 7,e:set 7,h:set 7,l:set 7,(hl):set 7,a::add ix,bc::add ix,de::ld ix,#1234:ld (#1234),ix:inc ix:inc xh:dec xh:ld xh,#12:add ix,ix:ld ix,(#1234):dec ix:inc xl:dec xl:ld xl,#12::inc (ix+#12):dec (ix+#12):ld (ix+#12),#34:add ix,sp::ld b,xh:ld b,xl:ld b,(ix+#12):ld c,xh:ld c,xl:ld c,(ix+#12):::ld d,xh:ld d,xl:ld d,(ix+#12):ld e,xh:ld e,xl:ld e,(ix+#12)::ld xh,b:ld xh,c:ld xh,d:ld xh,e:ld xh,xh:ld xh,xl:ld h,(ix+#12):ld xh,a:ld xl,b:ld xl,c:ld xl,d:ld xl,e:ld xl,xh:ld xl,xl:ld l,(ix+#12):ld xl,a::ld (ix+#12),b:ld (ix+#12),c:ld (ix+#12),d:ld (ix+#12),e:ld (ix+#12),h:ld (ix+#12),l:ld (ix+#12),a:ld a,xh:ld a,xl:ld a,(ix+#12)::add xh:add xl:add (ix+#12):adc xh:adc xl:adc (ix+#12)::sub xh:sub xl:sub (ix+#12):sbc xh:sbc xl:sbc (ix+#12)::and xh:and xl:and (ix+#12):xor xh:xor xl:xor (ix+#12)::or xh:or xl:or (ix+#12):cp xh:cp xl:cp (ix+#12)::pop ix:ex (sp),ix:push ix:jp (ix)::ld sp,ix:::rlc (ix+#12),b:rlc (ix+#12),c:rlc (ix+#12),d:rlc (ix+#12),e:rlc (ix+#12),h:rlc (ix+#12),l:rlc (ix+#12):rlc (ix+#12),a:rrc (ix+#12),b:rrc (ix+#12),c:rrc (ix+#12),d:rrc (ix+#12),e:rrc (ix+#12),h:rrc (ix+#12),l:rrc (ix+#12):rrc (ix+#12),a::rl (ix+#12),b:rl (ix+#12),c:rl (ix+#12),d:rl (ix+#12),e:rl (ix+#12),h:rl (ix+#12),l:rl (ix+#12):rl (ix+#12),a:rr (ix+#12),b:rr (ix+#12),c:rr (ix+#12),d:rr (ix+#12),e:rr (ix+#12),h:rr (ix+#12),l:rr (ix+#12):rr (ix+#12),a::sla (ix+#12),b:sla (ix+#12),c:sla (ix+#12),d:sla (ix+#12),e:sla (ix+#12),h:sla (ix+#12),l:sla (ix+#12):sla (ix+#12),a:sra (ix+#12),b:sra (ix+#12),c:sra (ix+#12),d:sra (ix+#12),e:sra (ix+#12),h:sra (ix+#12),l:sra (ix+#12):sra (ix+#12),a::sll (ix+#12),b:sll (ix+#12),c:sll (ix+#12),d:sll (ix+#12),e:sll (ix+#12),h:sll (ix+#12),l:sll (ix+#12):sll (ix+#12),a:srl (ix+#12),b:srl (ix+#12),c:srl (ix+#12),d:srl (ix+#12),e:srl (ix+#12),h:srl (ix+#12),l:srl (ix+#12):srl (ix+#12),a::bit 0,(ix+#12):bit 1,(ix+#12):bit 2,(ix+#12):bit 3,(ix+#12):bit 4,(ix+#12):bit 5,(ix+#12):bit 6,(ix+#12):bit 7,(ix+#12):bit 0,(ix+#12),d:bit 1,(ix+#12),b:bit 2,(ix+#12),c:bit 3,(ix+#12),d:bit 4,(ix+#12),e:bit 5,(ix+#12),h:bit 6,(ix+#12),l:bit 7,(ix+#12),a:::res 0,(ix+#12),b:res 0,(ix+#12),c:res 0,(ix+#12),d:res 0,(ix+#12),e:res 0,(ix+#12),h:res 0,(ix+#12),l:res 0,(ix+#12):res 0,(ix+#12),a::res 1,(ix+#12),b:res 1,(ix+#12),c:res 1,(ix+#12),d:res 1,(ix+#12),e:res 1,(ix+#12),h:res 1,(ix+#12),l:res 1,(ix+#12):res 1,(ix+#12),a::res 2,(ix+#12),b:res 2,(ix+#12),c:res 2,(ix+#12),d:res 2,(ix+#12),e:res 2,(ix+#12),h:res 2,(ix+#12),l:res 2,(ix+#12):res 2,(ix+#12),a::res 3,(ix+#12),b:res 3,(ix+#12),c:res 3,(ix+#12),d:res 3,(ix+#12),e:res 3,(ix+#12),h:res 3,(ix+#12),l:res 3,(ix+#12):res 3,(ix+#12),a::res 4,(ix+#12),b:res 4,(ix+#12),c:res 4,(ix+#12),d:res 4,(ix+#12),e:res 4,(ix+#12),h:res 4,(ix+#12),l:res 4,(ix+#12):res 4,(ix+#12),a::res 5,(ix+#12),b:res 5,(ix+#12),c:res 5,(ix+#12),d:res 5,(ix+#12),e:res 5,(ix+#12),h:res 5,(ix+#12),l:res 5,(ix+#12):res 5,(ix+#12),a::res 6,(ix+#12),b:res 6,(ix+#12),c:res 6,(ix+#12),d:res 6,(ix+#12),e:res 6,(ix+#12),h:res 6,(ix+#12),l:res 6,(ix+#12):res 6,(ix+#12),a::res 7,(ix+#12),b:res 7,(ix+#12),c:res 7,(ix+#12),d:res 7,(ix+#12),e:res 7,(ix+#12),h:res 7,(ix+#12),l:res 7,(ix+#12):res 7,(ix+#12),a::set 0,(ix+#12),b:set 0,(ix+#12),c:set 0,(ix+#12),d:set 0,(ix+#12),e:set 0,(ix+#12),h:set 0,(ix+#12),l:set 0,(ix+#12):set 0,(ix+#12),a::set 1,(ix+#12),b:set 1,(ix+#12),c:set 1,(ix+#12),d:set 1,(ix+#12),e:set 1,(ix+#12),h:set 1,(ix+#12),l:set 1,(ix+#12):set 1,(ix+#12),a::set 2,(ix+#12),b:set 2,(ix+#12),c:set 2,(ix+#12),d:set 2,(ix+#12),e:set 2,(ix+#12),h:set 2,(ix+#12),l:set 2,(ix+#12):set 2,(ix+#12),a::set 3,(ix+#12),b:set 3,(ix+#12),c:set 3,(ix+#12),d:set 3,(ix+#12),e:set 3,(ix+#12),h:set 3,(ix+#12),l:set 3,(ix+#12):set 3,(ix+#12),a::set 4,(ix+#12),b:set 4,(ix+#12),c:set 4,(ix+#12),d:set 4,(ix+#12),e:set 4,(ix+#12),h:set 4,(ix+#12),l:set 4,(ix+#12):set 4,(ix+#12),a::set 5,(ix+#12),b:set 5,(ix+#12),c:set 5,(ix+#12),d:set 5,(ix+#12),e:set 5,(ix+#12),h:set 5,(ix+#12),l:set 5,(ix+#12):set 5,(ix+#12),a::set 6,(ix+#12),b:set 6,(ix+#12),c:set 6,(ix+#12),d:set 6,(ix+#12),e:set 6,(ix+#12),h:set 6,(ix+#12),l:set 6,(ix+#12):set 6,(ix+#12),a::set 7,(ix+#12),b:set 7,(ix+#12),c:set 7,(ix+#12),d:set 7,(ix+#12),e:set 7,(ix+#12),h:set 7,(ix+#12),l:set 7,(ix+#12):set 7,(ix+#12),a::add iy,bc::add iy,de::ld iy,#1234:ld (#1234),iy:inc iy:inc yh:dec yh:ld yh,#12:add iy,iy:ld iy,(#1234):dec iy:inc yl:dec yl:ld yl,#12::inc (iy+#12):dec (iy+#12):ld (iy+#12),#34:add iy,sp::ld b,yh:ld b,yl:ld b,(iy+#12):ld c,yh:ld c,yl:ld c,(iy+#12):::ld d,yh:ld d,yl:ld d,(iy+#12):ld e,yh:ld e,yl:ld e,(iy+#12)::ld yh,b:ld yh,c:ld yh,d:ld yh,e:ld yh,yh:ld yh,yl:ld h,(iy+#12):ld yh,a:ld yl,b:ld yl,c:ld yl,d:ld yl,e:ld yl,yh:ld yl,yl:ld l,(iy+#12):ld yl,a::ld (iy+#12),b:ld (iy+#12),c:ld (iy+#12),d:ld (iy+#12),e:ld (iy+#12),h:ld (iy+#12),l:ld (iy+#12),a:ld a,yh:ld a,yl:ld a,(iy+#12)::add yh:add yl:add (iy+#12):adc yh:adc yl:adc (iy+#12)::sub yh:sub yl:sub (iy+#12):sbc yh:sbc yl:sbc (iy+#12)::and yh:and yl:and (iy+#12):xor yh:xor yl:xor (iy+#12)::or yh:or yl:or (iy+#12):cp yh:cp yl:cp (iy+#12)::pop iy:ex (sp),iy:push iy:jp (iy)::ld sp,iy::rlc (iy+#12),b:rlc (iy+#12),c:rlc (iy+#12),d:rlc (iy+#12),e:rlc (iy+#12),h:rlc (iy+#12),l:rlc (iy+#12):rlc (iy+#12),a:rrc (iy+#12),b:rrc (iy+#12),c:rrc (iy+#12),d:rrc (iy+#12),e:rrc (iy+#12),h:rrc (iy+#12),l:rrc (iy+#12):rrc (iy+#12),a::rl (iy+#12),b:rl (iy+#12),c:rl (iy+#12),d:rl (iy+#12),e:rl (iy+#12),h:rl (iy+#12),l:rl (iy+#12):rl (iy+#12),a:rr (iy+#12),b:rr (iy+#12),c:rr (iy+#12),d:rr (iy+#12),e:rr (iy+#12),h:rr (iy+#12),l:rr (iy+#12):rr (iy+#12),a::sla (iy+#12),b:sla (iy+#12),c:sla (iy+#12),d:sla (iy+#12),e:sla (iy+#12),h:sla (iy+#12),l:sla (iy+#12):sla (iy+#12),a:sra (iy+#12),b:sra (iy+#12),c:sra (iy+#12),d:sra (iy+#12),e:sra (iy+#12),h:sra (iy+#12),l:sra (iy+#12):sra (iy+#12),a::sll (iy+#12),b:sll (iy+#12),c:sll (iy+#12),d:sll (iy+#12),e:sll (iy+#12),h:sll (iy+#12),l:sll (iy+#12):sll (iy+#12),a:srl (iy+#12),b:srl (iy+#12),c:srl (iy+#12),d:srl (iy+#12),e:srl (iy+#12),h:srl (iy+#12),l:srl (iy+#12):srl (iy+#12),a::bit 0,(iy+#12):bit 1,(iy+#12):bit 2,(iy+#12):bit 3,(iy+#12):bit 4,(iy+#12):bit 5,(iy+#12):bit 6,(iy+#12):bit 7,(iy+#12)::res 0,(iy+#12),b:res 0,(iy+#12),c:res 0,(iy+#12),d:res 0,(iy+#12),e:res 0,(iy+#12),h:res 0,(iy+#12),l:res 0,(iy+#12):res 0,(iy+#12),a::res 1,(iy+#12),b:res 1,(iy+#12),c:res 1,(iy+#12),d:res 1,(iy+#12),e:res 1,(iy+#12),h:res 1,(iy+#12),l:res 1,(iy+#12):res 1,(iy+#12),a::res 2,(iy+#12),b:res 2,(iy+#12),c:res 2,(iy+#12),d:res 2,(iy+#12),e:res 2,(iy+#12),h:res 2,(iy+#12),l:res 2,(iy+#12):res 2,(iy+#12),a::res 3,(iy+#12),b:res 3,(iy+#12),c:res 3,(iy+#12),d:res 3,(iy+#12),e:res 3,(iy+#12),h:res 3,(iy+#12),l:res 3,(iy+#12):res 3,(iy+#12),a::res 4,(iy+#12),b:res 4,(iy+#12),c:res 4,(iy+#12),d:res 4,(iy+#12),e:res 4,(iy+#12),h:res 4,(iy+#12),l:res 4,(iy+#12):res 4,(iy+#12),a::res 5,(iy+#12),b:res 5,(iy+#12),c:res 5,(iy+#12),d:res 5,(iy+#12),e:res 5,(iy+#12),h:res 5,(iy+#12),l:res 5,(iy+#12):res 5,(iy+#12),a::res 6,(iy+#12),b:res 6,(iy+#12),c:res 6,(iy+#12),d:res 6,(iy+#12),e:res 6,(iy+#12),h:res 6,(iy+#12),l:res 6,(iy+#12):res 6,(iy+#12),a::res 7,(iy+#12),b:res 7,(iy+#12),c:res 7,(iy+#12),d:res 7,(iy+#12),e:res 7,(iy+#12),h:res 7,(iy+#12),l:res 7,(iy+#12):res 7,(iy+#12),a::set 0,(iy+#12),b:set 0,(iy+#12),c:set 0,(iy+#12),d:set 0,(iy+#12),e:set 0,(iy+#12),h:set 0,(iy+#12),l:set 0,(iy+#12):set 0,(iy+#12),a::set 1,(iy+#12),b:set 1,(iy+#12),c:set 1,(iy+#12),d:set 1,(iy+#12),e:set 1,(iy+#12),h:set 1,(iy+#12),l:set 1,(iy+#12):set 1,(iy+#12),a::set 2,(iy+#12),b:set 2,(iy+#12),c:set 2,(iy+#12),d:set 2,(iy+#12),e:set 2,(iy+#12),h:set 2,(iy+#12),l:set 2,(iy+#12):set 2,(iy+#12),a::set 3,(iy+#12),b:set 3,(iy+#12),c:set 3,(iy+#12),d:set 3,(iy+#12),e:set 3,(iy+#12),h:set 3,(iy+#12),l:set 3,(iy+#12):set 3,(iy+#12),a::set 4,(iy+#12),b:set 4,(iy+#12),c:set 4,(iy+#12),d:set 4,(iy+#12),e:set 4,(iy+#12),h:set 4,(iy+#12),l:set 4,(iy+#12):set 4,(iy+#12),a::set 5,(iy+#12),b:set 5,(iy+#12),c:set 5,(iy+#12),d:set 5,(iy+#12),e:set 5,(iy+#12),h:set 5,(iy+#12),l:set 5,(iy+#12):set 5,(iy+#12),a::set 6,(iy+#12),b:set 6,(iy+#12),c:set 6,(iy+#12),d:set 6,(iy+#12),e:set 6,(iy+#12),h:set 6,(iy+#12),l:set 6,(iy+#12):set 6,(iy+#12),a::set 7,(iy+#12),b:set 7,(iy+#12),c:set 7,(iy+#12),d:set 7,(iy+#12),e:set 7,(iy+#12),h:set 7,(iy+#12),l:set 7,(iy+#12):set 7,(iy+#12),a:"

void RasmAutotest(void)
{
	#undef FUNC
	#define FUNC "RasmAutotest"

	unsigned char *opcode=NULL;
	int opcodelen,ret;
	char *minicode;
	
	minicode=MemMalloc(4);minicode[0]=' ';minicode[1]='n';minicode[2]='o';minicode[3]='p';
	//int RasmAssemble(const char *datain, int lenin, unsigned char **dataout, int *lenout)
	ret=RasmAssemble(minicode,4,&opcode,&opcodelen);
	if (!ret && opcodelen==1 && opcode[0]==0x0) {} else {printf("Autotest 001 ERROR\n");exit(-1);}
	MemFree(opcode);opcode=NULL;
	ret=RasmAssemble("ld a,5\n",strlen("ld a,5\n"),&opcode,&opcodelen);
	if (!ret && opcodelen==2 && opcode[0]==0x3E && opcode[1]==5) {} else {printf("Autotest 002 ERROR\n");exit(-1);}
	MemFree(opcode);opcode=NULL;
	ret=RasmAssemble(AUTOTEST_MACRO,strlen(AUTOTEST_MACRO),&opcode,&opcodelen);
	if (!ret) {} else {printf("Autotest 003 ERROR (imbricated macros)\n");exit(-1);}
	MemFree(opcode);opcode=NULL;	
	ret=RasmAssemble(AUTOTEST_OPCODES,strlen(AUTOTEST_OPCODES),&opcode,&opcodelen);
	if (!ret) {} else {printf("Autotest 004 ERROR (all opcodes)\n");exit(-1);}
	MemFree(opcode);opcode=NULL;
	
	printf("All internal tests OK\n");
	exit(0);
}


/******************************************************
LZ48 v005 / LZ49 v002
******************************************************/
int LZ48_encode_extended_length(unsigned char *odata, int length)
{
	int ioutput=0;

	while (length>=255) {
		odata[ioutput++]=0xFF;
		length-=255;
	}
	/* if the last value is 255 we must encode 0 to end extended length */
	/*if (length==0) rasm_printf(ae,"bugfixed!\n");*/
	odata[ioutput++]=(unsigned char)length;
	return ioutput;
}

int LZ48_encode_block(unsigned char *odata,unsigned char *data, int literaloffset,int literalcpt,int offset,int maxlength)
{
	int ioutput=1;
	int token=0;
	int i;

	if (offset<0 || offset>255) {
		fprintf(stderr,"internal offset error!\n");
		exit(-2);
	}
	
	if (literalcpt<15) {
		token=literalcpt<<4; 
	} else {
		token=0xF0;
		ioutput+=LZ48_encode_extended_length(odata+ioutput,literalcpt-15);
	}

	for (i=0;i<literalcpt;i++) odata[ioutput++]=data[literaloffset++];

	if (maxlength<18) {
		if (maxlength>2) {
			token|=(maxlength-3);
		} else {
			/* endoffset has no length */
		}
	} else {
		token|=0xF;
		ioutput+=LZ48_encode_extended_length(odata+ioutput,maxlength-18);
	}

	odata[ioutput++]=(unsigned char)offset-1;
	
	odata[0]=(unsigned char)token;
	return ioutput;
}

unsigned char *LZ48_encode_legacy(unsigned char *data, int length, int *retlength)
{
	int i,startscan,current=1,token,ioutput=1,curscan;
	int maxoffset=0,maxlength,matchlength,literal=0,literaloffset=1;
	unsigned char *odata=NULL;
	
	odata=MemMalloc((size_t)length*1.5+10);
	if (!odata) {
		fprintf(stderr,"malloc(%.0lf) - memory full\n",(size_t)length*1.5+10);
		exit(-1);
	}

	/* first byte always literal */
	odata[0]=data[0];

	/* force short data encoding */
	if (length<5) {
		token=(length-1)<<4;
		odata[ioutput++]=(unsigned char)token;
		for (i=1;i<length;i++) odata[ioutput++]=data[current++];
		odata[ioutput++]=0xFF;
		*retlength=ioutput;
		return odata;
	}

	while (current<length) {
		maxlength=0;
		startscan=current-255;
		if (startscan<0) startscan=0;
		while (startscan<current) {
			matchlength=0;
			curscan=current;
			for (i=startscan;curscan<length;i++) {
				if (data[i]==data[curscan++]) matchlength++; else break;
			}
			if (matchlength>=3 && matchlength>maxlength) {
				maxoffset=startscan;
				maxlength=matchlength;
			}
			startscan++;
		}
		if (maxlength) {
			ioutput+=LZ48_encode_block(odata+ioutput,data,literaloffset,literal,current-maxoffset,maxlength);
			current+=maxlength;
			literaloffset=current;
			literal=0;
		} else {
			literal++;
			current++;
		}
	}
	ioutput+=LZ48_encode_block(odata+ioutput,data,literaloffset,literal,0,0);
	*retlength=ioutput;
	return odata;
}

int LZ49_encode_extended_length(unsigned char *odata, int length)
{
	int ioutput=0;

	while (length>=255) {
		odata[ioutput++]=0xFF;
		length-=255;
	}
	/* if the last value is 255 we must encode 0 to end extended length */
	/*if (length==0) rasm_printf(ae,"bugfixed!\n");*/
	odata[ioutput++]=(unsigned char)length;
	return ioutput;
}

int LZ49_encode_block(unsigned char *odata,unsigned char *data, int literaloffset,int literalcpt,int offset,int maxlength)
{
	int ioutput=1;
	int token=0;
	int i;

	if (offset<0 || offset>511) {
		fprintf(stderr,"internal offset error!\n");
		exit(-2);
	}
	
	if (literalcpt<7) {
		token=literalcpt<<4; 
	} else {
		token=0x70;
		ioutput+=LZ49_encode_extended_length(odata+ioutput,literalcpt-7);
	}

	for (i=0;i<literalcpt;i++) odata[ioutput++]=data[literaloffset++];

	if (maxlength<18) {
		if (maxlength>2) {
			token|=(maxlength-3);
		} else {
			/* endoffset has no length */
		}
	} else {
		token|=0xF;
		ioutput+=LZ49_encode_extended_length(odata+ioutput,maxlength-18);
	}

	if (offset>255) {
		token|=0x80;
		offset-=256;
	}	
	odata[ioutput++]=(unsigned char)offset-1;
	
	odata[0]=(unsigned char)token;
	return ioutput;
}

unsigned char *LZ49_encode_legacy(unsigned char *data, int length, int *retlength)
{
	int i,startscan,current=1,token,ioutput=1,curscan;
	int maxoffset=0,maxlength,matchlength,literal=0,literaloffset=1;
	unsigned char *odata=NULL;
	
	odata=MemMalloc((size_t)(length*1.5+10));
	if (!odata) {
		fprintf(stderr,"malloc(%.0lf) - memory full\n",(size_t)length*1.5+10);
		exit(-1);
	}

	/* first byte always literal */
	odata[0]=data[0];

	/* force short data encoding */
	if (length<5) {
		token=(length-1)<<4;
		odata[ioutput++]=(unsigned char)token;
		for (i=1;i<length;i++) odata[ioutput++]=data[current++];
		odata[ioutput++]=0xFF;
		*retlength=ioutput;
		return odata;
	}

	while (current<length) {
		maxlength=0;
		startscan=current-511;
		if (startscan<0) startscan=0;
		while (startscan<current) {
			matchlength=0;
			curscan=current;
			for (i=startscan;curscan<length;i++) {
				if (data[i]==data[curscan++]) matchlength++; else break;
			}
			if (matchlength>=3 && matchlength>maxlength) {
				maxoffset=startscan;
				maxlength=matchlength;
			}
			startscan++;
		}
		if (maxlength) {
			ioutput+=LZ49_encode_block(odata+ioutput,data,literaloffset,literal,current-maxoffset,maxlength);
			current+=maxlength;
			literaloffset=current;
			literal=0;
		} else {
			literal++;
			current++;
		}
	}
	ioutput+=LZ49_encode_block(odata+ioutput,data,literaloffset,literal,0,0);
	*retlength=ioutput;
	return odata;
}


/***************************************
	semi-generic body of program
***************************************/

#ifndef INTEGRATED_ASSEMBLY

/*
	Usage
	display the mandatory parameters
*/
void Usage()
{
	#undef FUNC
	#define FUNC "Usage"
	
	printf("%.*s.exe v0.61 (c) 2017 Edouard BERGE\n",(int)(sizeof(__FILENAME__)-3),__FILENAME__);
	#ifndef NO_3RD_PARTIES
	printf("LZ4 (c) 2011 by Yann Collet\n");
	printf("ZX7 (c) 2012 by Einar Saukas\n");
	printf("Exomizer 2 (c) 2005 by Magnus Lind\n");
	#endif
	printf("(use -n option to display all licenses)\n");
	printf("\n");
	printf("syntax: rasm <inputfile> [options]\n");
	printf("\n");
	printf("filename options:\n");
	printf("-o  <outputfile radix>   choose a common radix for every files (bin,cpr,sym)\n");
	printf("-ob <binary filename>    choose a full filename for binary output\n");
	printf("-oc <cartridge filename> choose a full filename for cartridge output\n");
	printf("-os <symbol filename>    choose a full filename for symbol output\n");
	printf("\n");
	printf("symbol options:\n");
	printf("-s  export symbols %%s #%%X B%%d (label,adr,cprbank)\n");
	printf("-sp export symbols with Pasmo convention\n");
	printf("-sw export symbols with Winape convention\n");
	printf("-sl export also local symbol (add to another -s option)\n");
	printf("-sv export also variables symbol (add to another -s option)\n");
	printf("-sq export also EQU symbol (add to another -s option)\n");
	printf("-l <labelfile> import symbol file (winape,pasmo,rasm)\n");
	printf("\n");
	printf("compatibility option:\n");
	printf("-m  maxam style calculations\n");
	printf("\n");
	printf("cartridge option:\n");
	printf("-c  cartridge summary\n");
	printf("\n");
	printf("developper options:\n");
	printf("-v  verbose mode\n");
	printf("-d  pre-processing verbose mode\n");
	printf("-a  assembly verbose mode\n");
	printf("-n  display RASM & 3rd parties licenses\n");
	printf("\n");
	
	exit(ABORT_ERROR);
}

void Licenses()
{
	#undef FUNC
	#define FUNC "Licenses"
	
printf("RASM is using MIT 'expat' license\n");
printf("« Copyright © BERGE Edouard (roudoudou)\n\n");

printf("Permission  is  hereby  granted,  free  of charge,\n");
printf("to any person obtaining a copy  of  this  software\n");
printf("and  associated  documentation/source   files   of\n");
printf("RASM, to deal in the Software without restriction,\n");
printf("including without limitation the  rights  to  use,\n");
printf("copy,   modify,   merge,   publish,    distribute,\n");
printf("sublicense,  and/or  sell  copies of the Software,\n");
printf("and  to  permit  persons  to  whom the Software is\n");
printf("furnished  to  do  so,  subject  to  the following\n");
printf("conditions:\n");

printf("The above copyright  notice  and  this  permission\n");
printf("notice   shall   be  included  in  all  copies  or\n");
printf("substantial portions of the Software.\n");
printf("The   Software   is   provided  'as is',   without\n");
printf("warranty   of   any   kind,  express  or  implied,\n");
printf("including  but  not  limited  to the warranties of\n");
printf("merchantability,   fitness   for   a    particular\n");
printf("purpose  and  noninfringement.  In  no event shall\n");
printf("the  authors  or  copyright  holders be liable for\n");
printf("any  claim, damages  or other  liability,  whether\n");
printf("in  an  action  of  contract, tort  or  otherwise,\n");
printf("arising from,  out of  or in connection  with  the\n");
printf("software  or  the  use  or  other  dealings in the\n");
printf("Software. »\n");

#ifndef NO_3RD_PARTIES
printf("\n\n\n\n");
printf("******* license of LZ4 cruncher / sources were modified ***********\n\n\n\n");

printf("BSD 2-Clause License (http://www.opensource.org/licenses/bsd-license.php)\n");

printf("Redistribution and use in source and binary forms, with or without\n");
printf("modification, are permitted provided that the following conditions are\n");
printf("met:\n\n");

printf("    * Redistributions of source code must retain the above copyright\n");
printf("notice, this list of conditions and the following disclaimer.\n");
printf("    * Redistributions in binary form must reproduce the above\n");
printf("copyright notice, this list of conditions and the following disclaimer\n");
printf("in the documentation and/or other materials provided with the\n");
printf("distribution.\n\n");

printf("THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS\n");
printf("'AS IS' AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT\n");
printf("LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR\n");
printf("A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT\n");
printf("OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,\n");
printf("SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT\n");
printf("LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,\n");
printf("DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY\n");
printf("THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT\n");
printf("(INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE\n");
printf("OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.\n\n");

printf("You can contact the author at :\n");
printf(" - LZ4 homepage : http://www.lz4.org\n");
printf(" - LZ4 source repository : https://github.com/lz4/lz4\n");


printf("\n\n\n\n");
printf("******* license of ZX7 cruncher / sources were modified ***********\n\n\n\n");


printf(" * (c) Copyright 2012 by Einar Saukas. All rights reserved.\n");
printf(" *\n");
printf(" * Redistribution and use in source and binary forms, with or without\n");
printf(" * modification, are permitted provided that the following conditions are met:\n");
printf(" *     * Redistributions of source code must retain the above copyright\n");
printf(" *       notice, this list of conditions and the following disclaimer.\n");
printf(" *     * Redistributions in binary form must reproduce the above copyright\n");
printf(" *       notice, this list of conditions and the following disclaimer in the\n");
printf(" *       documentation and/or other materials provided with the distribution.\n");
printf(" *     * The name of its author may not be used to endorse or promote products\n");
printf(" *       derived from this software without specific prior written permission.\n");
printf(" *\n");
printf(" * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS 'AS IS' AND\n");
printf(" * ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED\n");
printf(" * WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE\n");
printf(" * DISCLAIMED. IN NO EVENT SHALL <COPYRIGHT HOLDER> BE LIABLE FOR ANY\n");
printf(" * DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES\n");
printf(" * (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;\n");
printf(" * LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND\n");
printf(" * ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT\n");
printf(" * (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS\n");
printf(" * SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.\n");


printf("\n\n\n\n");
printf("******* license of exomizer cruncher / sources were modified ***********\n\n\n\n");


printf(" * Copyright (c) 2005 Magnus Lind.\n");
printf(" *\n");
printf(" * This software is provided 'as-is', without any express or implied warranty.\n");
printf(" * In no event will the authors be held liable for any damages arising from\n");
printf(" * the use of this software.\n");
printf(" *\n");
printf(" * Permission is granted to anyone to use this software, alter it and re-\n");
printf(" * distribute it freely for any non-commercial, non-profit purpose subject to\n");
printf(" * the following restrictions:\n");
printf(" *\n");
printf(" *   1. The origin of this software must not be misrepresented; you must not\n");
printf(" *   claim that you wrote the original software. If you use this software in a\n");
printf(" *   product, an acknowledgment in the product documentation would be\n");
printf(" *   appreciated but is not required.\n");
printf(" *\n");
printf(" *   2. Altered source versions must be plainly marked as such, and must not\n");
printf(" *   be misrepresented as being the original software.\n");
printf(" *\n");
printf(" *   3. This notice may not be removed or altered from any distribution.\n");
printf(" *\n");
printf(" *   4. The names of this software and/or it's copyright holders may not be\n");
printf(" *   used to endorse or promote products derived from this software without\n");
printf(" *   specific prior written permission.\n");
#endif

printf("\n\n");



	exit(0);
}

/*
	ParseOptions
	
	used to parse command line and configuration file
*/
int ParseOptions(char **argv,int argc,char **filename, int *export_sym, int *verbose, char **outputfilename, float *rough, int *export_local, char **labelfilename, int *export_var, int *export_equ, char **symbol_name, char **binary_name, char **cartridge_name)
{
	#undef FUNC
	#define FUNC "ParseOptions"
	int i=0;

	if (strcmp(argv[i],"-autotest")==0) {
		RasmAutotest();
	}
	
	if (argv[i][0]=='-')
	{
		switch(argv[i][1])
		{
			case 'S':
			case 's':
				switch (argv[i][2]) {
					case 0:*export_sym=1;return 0;
					case 'p':
					case 'P':
						*export_sym=2;return 0;
					case 'w':
					case 'W':
						*export_sym=3;return 0;
					case 'l':
					case 'L':
						*export_local=1;return 0;
					case 'v':
					case 'V':
						*export_var=1;return 0;
					case 'q':
					case 'Q':
						*export_equ=1;return 0;
					default:
					break;
				}
				Usage();
			case 'L':
			case 'l':
				if (i+1<argc && *labelfilename==NULL) {
					*labelfilename=argv[++i];
					break;
				}	
				Usage();
			case 'I':
			case 'i':
				if (i+1<argc && *filename==NULL) {
					*filename=argv[++i];
					break;
				}	
				Usage();
			case 'O':
			case 'o':
				switch (argv[i][2]) {
					case 0:
						if (i+1<argc && *outputfilename==NULL) {
							*outputfilename=argv[++i];
							break;
						}	
						Usage();
					case 'b':
						if (i+1<argc && *binary_name==NULL) {
							*binary_name=argv[++i];
							break;
						}
						Usage();
					case 'c':
						if (i+1<argc && *cartridge_name==NULL) {
							*cartridge_name=argv[++i];
							break;
						}
						Usage();
					case 's':
						if (i+1<argc && *symbol_name==NULL) {
							*symbol_name=argv[++i];
							break;
						}
						Usage();
					default:
						Usage();
				}
				break;
			case 'M':
			case 'm':*rough=0.0;
				break;
			case 'D':
			case 'd':*verbose|=2;
				break;
			case 'A':
			case 'a':*verbose|=4;
				break;
			case 'C':
			case 'c':*verbose|=8;
				break;
			case 'V':
			case 'v':*verbose=1;
				break;
			case 'N':
			case 'n':Licenses();
			case 'H':
			case 'h':Usage();
			default:
				Usage();				
		}
	} else {
		if (*filename==NULL) {
			*filename=argv[i];
		} else if (*outputfilename==NULL) {
			*outputfilename=argv[i];
		} else Usage();
	}
	return i;
}

/*
	GetParametersFromCommandLine	
	retrieve parameters from command line and fill pointers to file names
*/
void GetParametersFromCommandLine(int argc, char **argv, char **filename, int *export_sym, int *verbose, char **outputfilename, float *rough, int *export_local, char **labelfilename, int *export_var, int *export_equ, char **symbol_name, char **binary_name, char **cartridge_name)
{
	#undef FUNC
	#define FUNC "GetParametersFromCommandLine"
	int i;
	
	for (i=1;i<argc;i++)
		i+=ParseOptions(&argv[i],argc-i,filename,export_sym,verbose,outputfilename,rough,export_local,labelfilename,export_var,export_equ,symbol_name,binary_name,cartridge_name);

	if (!*filename) Usage();
	if (*export_local && !*export_sym) Usage();
}

/*
	main
	
	check parameters
	execute the main processing
*/


int main(int argc, char **argv)
{
	#undef FUNC
	#define FUNC "main"

	char *labelfilename=NULL;
	char *filename=NULL;
	char *outputfilename=NULL;
	int export_local=0;
	int export_var=0;
	int export_equ=0;
	int export_sym=0;
	int verbose=0;
	float rough=0.5;
	char *symbol_name=NULL;
	char *binary_name=NULL;
	char *cartridge_name=NULL;

	GetParametersFromCommandLine(argc,argv,&filename,&export_sym,&verbose,&outputfilename,&rough,&export_local,&labelfilename,&export_var,&export_equ,&symbol_name,&binary_name,&cartridge_name);
	Rasm(filename,export_sym,verbose,outputfilename,rough,export_local,labelfilename,export_var,export_equ,symbol_name,binary_name,cartridge_name);
	#ifdef RDD
	/* private dev lib tools */
	CloseLibrary();
	#endif
	exit(0);
	return 0; // Open WATCOM Warning on this...
}

#endif



