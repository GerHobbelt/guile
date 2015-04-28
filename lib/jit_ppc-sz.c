
#if __WORDSIZE == 32
#if defined(__ppc__)
#define JIT_INSTR_MAX 44
    0,	/* data */
    0,	/* live */
    0,	/* align */
    0,	/* save */
    0,	/* load */
    0,	/* #name */
    0,	/* #note */
    0,	/* label */
    44,	/* prolog */
    0,	/* arg */
    4,	/* addr */
    12,	/* addi */
    4,	/* addcr */
    12,	/* addci */
    4,	/* addxr */
    8,	/* addxi */
    4,	/* subr */
    12,	/* subi */
    4,	/* subcr */
    12,	/* subci */
    4,	/* subxr */
    8,	/* subxi */
    16,	/* rsbi */
    4,	/* mulr */
    12,	/* muli */
    12,	/* qmulr */
    16,	/* qmuli */
    12,	/* qmulr_u */
    16,	/* qmuli_u */
    4,	/* divr */
    12,	/* divi */
    4,	/* divr_u */
    12,	/* divi_u */
    20,	/* qdivr */
    16,	/* qdivi */
    20,	/* qdivr_u */
    16,	/* qdivi_u */
    12,	/* remr */
    20,	/* remi */
    12,	/* remr_u */
    20,	/* remi_u */
    4,	/* andr */
    12,	/* andi */
    4,	/* orr */
    12,	/* ori */
    4,	/* xorr */
    12,	/* xori */
    4,	/* lshr */
    4,	/* lshi */
    4,	/* rshr */
    4,	/* rshi */
    4,	/* rshr_u */
    4,	/* rshi_u */
    4,	/* negr */
    4,	/* comr */
    12,	/* ltr */
    12,	/* lti */
    12,	/* ltr_u */
    16,	/* lti_u */
    16,	/* ler */
    16,	/* lei */
    16,	/* ler_u */
    16,	/* lei_u */
    12,	/* eqr */
    12,	/* eqi */
    16,	/* ger */
    16,	/* gei */
    16,	/* ger_u */
    16,	/* gei_u */
    12,	/* gtr */
    12,	/* gti */
    12,	/* gtr_u */
    12,	/* gti_u */
    16,	/* ner */
    16,	/* nei */
    4,	/* movr */
    8,	/* movi */
    4,	/* extr_c */
    4,	/* extr_uc */
    4,	/* extr_s */
    4,	/* extr_us */
    0,	/* extr_i */
    0,	/* extr_ui */
    4,	/* htonr_us */
    4,	/* htonr_ui */
    0,	/* htonr_ul */
    8,	/* ldr_c */
    12,	/* ldi_c */
    4,	/* ldr_uc */
    8,	/* ldi_uc */
    4,	/* ldr_s */
    8,	/* ldi_s */
    4,	/* ldr_us */
    8,	/* ldi_us */
    4,	/* ldr_i */
    8,	/* ldi_i */
    0,	/* ldr_ui */
    0,	/* ldi_ui */
    0,	/* ldr_l */
    0,	/* ldi_l */
    8,	/* ldxr_c */
    8,	/* ldxi_c */
    4,	/* ldxr_uc */
    4,	/* ldxi_uc */
    4,	/* ldxr_s */
    4,	/* ldxi_s */
    4,	/* ldxr_us */
    4,	/* ldxi_us */
    4,	/* ldxr_i */
    4,	/* ldxi_i */
    0,	/* ldxr_ui */
    0,	/* ldxi_ui */
    0,	/* ldxr_l */
    0,	/* ldxi_l */
    4,	/* str_c */
    8,	/* sti_c */
    4,	/* str_s */
    8,	/* sti_s */
    4,	/* str_i */
    8,	/* sti_i */
    0,	/* str_l */
    0,	/* sti_l */
    4,	/* stxr_c */
    4,	/* stxi_c */
    4,	/* stxr_s */
    4,	/* stxi_s */
    4,	/* stxr_i */
    4,	/* stxi_i */
    0,	/* stxr_l */
    0,	/* stxi_l */
    8,	/* bltr */
    8,	/* blti */
    8,	/* bltr_u */
    12,	/* blti_u */
    8,	/* bler */
    8,	/* blei */
    8,	/* bler_u */
    12,	/* blei_u */
    8,	/* beqr */
    16,	/* beqi */
    8,	/* bger */
    8,	/* bgei */
    8,	/* bger_u */
    8,	/* bgei_u */
    8,	/* bgtr */
    8,	/* bgti */
    8,	/* bgtr_u */
    8,	/* bgti_u */
    8,	/* bner */
    16,	/* bnei */
    12,	/* bmsr */
    12,	/* bmsi */
    12,	/* bmcr */
    12,	/* bmci */
    12,	/* boaddr */
    16,	/* boaddi */
    12,	/* boaddr_u */
    12,	/* boaddi_u */
    12,	/* bxaddr */
    16,	/* bxaddi */
    12,	/* bxaddr_u */
    12,	/* bxaddi_u */
    12,	/* bosubr */
    16,	/* bosubi */
    12,	/* bosubr_u */
    16,	/* bosubi_u */
    12,	/* bxsubr */
    16,	/* bxsubi */
    12,	/* bxsubr_u */
    16,	/* bxsubi_u */
    0,	/* jmpr */
    4,	/* jmpi */
    8,	/* callr */
    16,	/* calli */
    44,	/* epilog */
    0,	/* arg_f */
    4,	/* addr_f */
    12,	/* addi_f */
    4,	/* subr_f */
    12,	/* subi_f */
    12,	/* rsbi_f */
    4,	/* mulr_f */
    12,	/* muli_f */
    4,	/* divr_f */
    12,	/* divi_f */
    4,	/* negr_f */
    4,	/* absr_f */
    4,	/* sqrtr_f */
    12,	/* ltr_f */
    20,	/* lti_f */
    16,	/* ler_f */
    24,	/* lei_f */
    12,	/* eqr_f */
    20,	/* eqi_f */
    16,	/* ger_f */
    24,	/* gei_f */
    12,	/* gtr_f */
    20,	/* gti_f */
    16,	/* ner_f */
    24,	/* nei_f */
    16,	/* unltr_f */
    24,	/* unlti_f */
    16,	/* unler_f */
    24,	/* unlei_f */
    16,	/* uneqr_f */
    24,	/* uneqi_f */
    16,	/* unger_f */
    24,	/* ungei_f */
    16,	/* ungtr_f */
    24,	/* ungti_f */
    16,	/* ltgtr_f */
    24,	/* ltgti_f */
    16,	/* ordr_f */
    24,	/* ordi_f */
    12,	/* unordr_f */
    20,	/* unordi_f */
    12,	/* truncr_f_i */
    0,	/* truncr_f_l */
    20,	/* extr_f */
    4,	/* extr_d_f */
    4,	/* movr_f */
    8,	/* movi_f */
    4,	/* ldr_f */
    8,	/* ldi_f */
    4,	/* ldxr_f */
    4,	/* ldxi_f */
    4,	/* str_f */
    8,	/* sti_f */
    4,	/* stxr_f */
    4,	/* stxi_f */
    8,	/* bltr_f */
    16,	/* blti_f */
    12,	/* bler_f */
    20,	/* blei_f */
    8,	/* beqr_f */
    16,	/* beqi_f */
    12,	/* bger_f */
    20,	/* bgei_f */
    8,	/* bgtr_f */
    16,	/* bgti_f */
    8,	/* bner_f */
    16,	/* bnei_f */
    12,	/* bunltr_f */
    20,	/* bunlti_f */
    8,	/* bunler_f */
    16,	/* bunlei_f */
    12,	/* buneqr_f */
    20,	/* buneqi_f */
    8,	/* bunger_f */
    16,	/* bungei_f */
    12,	/* bungtr_f */
    20,	/* bungti_f */
    12,	/* bltgtr_f */
    20,	/* bltgti_f */
    8,	/* bordr_f */
    16,	/* bordi_f */
    8,	/* bunordr_f */
    16,	/* bunordi_f */
    0,	/* arg_d */
    4,	/* addr_d */
    12,	/* addi_d */
    4,	/* subr_d */
    12,	/* subi_d */
    12,	/* rsbi_d */
    4,	/* mulr_d */
    12,	/* muli_d */
    4,	/* divr_d */
    12,	/* divi_d */
    4,	/* negr_d */
    4,	/* absr_d */
    4,	/* sqrtr_d */
    12,	/* ltr_d */
    20,	/* lti_d */
    16,	/* ler_d */
    24,	/* lei_d */
    12,	/* eqr_d */
    20,	/* eqi_d */
    16,	/* ger_d */
    24,	/* gei_d */
    12,	/* gtr_d */
    20,	/* gti_d */
    16,	/* ner_d */
    24,	/* nei_d */
    16,	/* unltr_d */
    24,	/* unlti_d */
    16,	/* unler_d */
    24,	/* unlei_d */
    16,	/* uneqr_d */
    24,	/* uneqi_d */
    16,	/* unger_d */
    24,	/* ungei_d */
    16,	/* ungtr_d */
    24,	/* ungti_d */
    16,	/* ltgtr_d */
    24,	/* ltgti_d */
    16,	/* ordr_d */
    24,	/* ordi_d */
    12,	/* unordr_d */
    20,	/* unordi_d */
    12,	/* truncr_d_i */
    0,	/* truncr_d_l */
    20,	/* extr_d */
    4,	/* extr_f_d */
    4,	/* movr_d */
    8,	/* movi_d */
    4,	/* ldr_d */
    8,	/* ldi_d */
    4,	/* ldxr_d */
    4,	/* ldxi_d */
    4,	/* str_d */
    8,	/* sti_d */
    4,	/* stxr_d */
    4,	/* stxi_d */
    8,	/* bltr_d */
    16,	/* blti_d */
    12,	/* bler_d */
    20,	/* blei_d */
    8,	/* beqr_d */
    16,	/* beqi_d */
    12,	/* bger_d */
    20,	/* bgei_d */
    8,	/* bgtr_d */
    16,	/* bgti_d */
    8,	/* bner_d */
    16,	/* bnei_d */
    12,	/* bunltr_d */
    20,	/* bunlti_d */
    8,	/* bunler_d */
    16,	/* bunlei_d */
    12,	/* buneqr_d */
    20,	/* buneqi_d */
    8,	/* bunger_d */
    16,	/* bungei_d */
    12,	/* bungtr_d */
    20,	/* bungti_d */
    12,	/* bltgtr_d */
    20,	/* bltgti_d */
    8,	/* bordr_d */
    16,	/* bordi_d */
    8,	/* bunordr_d */
    16,	/* bunordi_d */
    0,	/* movr_w_f */
    0,	/* movr_ww_d */
    0,	/* movr_w_d */
    0,	/* movr_f_w */
    0,	/* movi_f_w */
    0,	/* movr_d_ww */
    0,	/* movi_d_ww */
    0,	/* movr_d_w */
    0,	/* movi_d_w */
    0,	/* x86_retval_f */
    0,	/* x86_retval_d */
    0,	/* va_start */
    0,	/* va_arg */
    0,	/* va_arg_d */
    0,	/* va_end */
#endif /* __ppc__ */
#endif /* __WORDSIZE */

#if __WORDSIZE == 32
#if defined(__powerpc__)
#define JIT_INSTR_MAX 72
    0,	/* data */
    0,	/* live */
    0,	/* align */
    0,	/* save */
    0,	/* load */
    0,	/* #name */
    0,	/* #note */
    0,	/* label */
    72,	/* prolog */
    0,	/* arg */
    4,	/* addr */
    12,	/* addi */
    4,	/* addcr */
    12,	/* addci */
    4,	/* addxr */
    8,	/* addxi */
    4,	/* subr */
    12,	/* subi */
    4,	/* subcr */
    12,	/* subci */
    4,	/* subxr */
    8,	/* subxi */
    16,	/* rsbi */
    4,	/* mulr */
    12,	/* muli */
    12,	/* qmulr */
    16,	/* qmuli */
    12,	/* qmulr_u */
    16,	/* qmuli_u */
    4,	/* divr */
    12,	/* divi */
    4,	/* divr_u */
    12,	/* divi_u */
    20,	/* qdivr */
    16,	/* qdivi */
    20,	/* qdivr_u */
    16,	/* qdivi_u */
    12,	/* remr */
    20,	/* remi */
    12,	/* remr_u */
    20,	/* remi_u */
    4,	/* andr */
    12,	/* andi */
    4,	/* orr */
    12,	/* ori */
    4,	/* xorr */
    12,	/* xori */
    4,	/* lshr */
    4,	/* lshi */
    4,	/* rshr */
    4,	/* rshi */
    4,	/* rshr_u */
    4,	/* rshi_u */
    4,	/* negr */
    4,	/* comr */
    12,	/* ltr */
    12,	/* lti */
    12,	/* ltr_u */
    16,	/* lti_u */
    16,	/* ler */
    16,	/* lei */
    16,	/* ler_u */
    16,	/* lei_u */
    12,	/* eqr */
    12,	/* eqi */
    16,	/* ger */
    16,	/* gei */
    16,	/* ger_u */
    16,	/* gei_u */
    12,	/* gtr */
    12,	/* gti */
    12,	/* gtr_u */
    12,	/* gti_u */
    16,	/* ner */
    16,	/* nei */
    4,	/* movr */
    8,	/* movi */
    4,	/* extr_c */
    4,	/* extr_uc */
    4,	/* extr_s */
    4,	/* extr_us */
    0,	/* extr_i */
    0,	/* extr_ui */
    4,	/* htonr_us */
    4,	/* htonr_ui */
    0,	/* htonr_ul */
    8,	/* ldr_c */
    12,	/* ldi_c */
    4,	/* ldr_uc */
    8,	/* ldi_uc */
    4,	/* ldr_s */
    8,	/* ldi_s */
    4,	/* ldr_us */
    8,	/* ldi_us */
    4,	/* ldr_i */
    8,	/* ldi_i */
    0,	/* ldr_ui */
    0,	/* ldi_ui */
    0,	/* ldr_l */
    0,	/* ldi_l */
    8,	/* ldxr_c */
    8,	/* ldxi_c */
    4,	/* ldxr_uc */
    4,	/* ldxi_uc */
    4,	/* ldxr_s */
    4,	/* ldxi_s */
    4,	/* ldxr_us */
    4,	/* ldxi_us */
    4,	/* ldxr_i */
    4,	/* ldxi_i */
    0,	/* ldxr_ui */
    0,	/* ldxi_ui */
    0,	/* ldxr_l */
    0,	/* ldxi_l */
    4,	/* str_c */
    8,	/* sti_c */
    4,	/* str_s */
    8,	/* sti_s */
    4,	/* str_i */
    8,	/* sti_i */
    0,	/* str_l */
    0,	/* sti_l */
    4,	/* stxr_c */
    4,	/* stxi_c */
    4,	/* stxr_s */
    4,	/* stxi_s */
    4,	/* stxr_i */
    4,	/* stxi_i */
    0,	/* stxr_l */
    0,	/* stxi_l */
    8,	/* bltr */
    8,	/* blti */
    8,	/* bltr_u */
    12,	/* blti_u */
    8,	/* bler */
    8,	/* blei */
    8,	/* bler_u */
    12,	/* blei_u */
    8,	/* beqr */
    16,	/* beqi */
    8,	/* bger */
    8,	/* bgei */
    8,	/* bger_u */
    8,	/* bgei_u */
    8,	/* bgtr */
    8,	/* bgti */
    8,	/* bgtr_u */
    8,	/* bgti_u */
    8,	/* bner */
    16,	/* bnei */
    12,	/* bmsr */
    12,	/* bmsi */
    12,	/* bmcr */
    12,	/* bmci */
    12,	/* boaddr */
    16,	/* boaddi */
    12,	/* boaddr_u */
    12,	/* boaddi_u */
    12,	/* bxaddr */
    16,	/* bxaddi */
    12,	/* bxaddr_u */
    12,	/* bxaddi_u */
    12,	/* bosubr */
    16,	/* bosubi */
    12,	/* bosubr_u */
    16,	/* bosubi_u */
    12,	/* bxsubr */
    16,	/* bxsubi */
    12,	/* bxsubr_u */
    16,	/* bxsubi_u */
    0,	/* jmpr */
    4,	/* jmpi */
    28,	/* callr */
    40,	/* calli */
    72,	/* epilog */
    0,	/* arg_f */
    4,	/* addr_f */
    16,	/* addi_f */
    4,	/* subr_f */
    16,	/* subi_f */
    16,	/* rsbi_f */
    4,	/* mulr_f */
    16,	/* muli_f */
    4,	/* divr_f */
    16,	/* divi_f */
    4,	/* negr_f */
    4,	/* absr_f */
    4,	/* sqrtr_f */
    12,	/* ltr_f */
    24,	/* lti_f */
    16,	/* ler_f */
    28,	/* lei_f */
    12,	/* eqr_f */
    24,	/* eqi_f */
    16,	/* ger_f */
    28,	/* gei_f */
    12,	/* gtr_f */
    24,	/* gti_f */
    16,	/* ner_f */
    28,	/* nei_f */
    16,	/* unltr_f */
    28,	/* unlti_f */
    16,	/* unler_f */
    28,	/* unlei_f */
    16,	/* uneqr_f */
    28,	/* uneqi_f */
    16,	/* unger_f */
    28,	/* ungei_f */
    16,	/* ungtr_f */
    28,	/* ungti_f */
    16,	/* ltgtr_f */
    28,	/* ltgti_f */
    16,	/* ordr_f */
    28,	/* ordi_f */
    12,	/* unordr_f */
    24,	/* unordi_f */
    12,	/* truncr_f_i */
    0,	/* truncr_f_l */
    20,	/* extr_f */
    4,	/* extr_d_f */
    4,	/* movr_f */
    12,	/* movi_f */
    4,	/* ldr_f */
    8,	/* ldi_f */
    4,	/* ldxr_f */
    4,	/* ldxi_f */
    4,	/* str_f */
    8,	/* sti_f */
    4,	/* stxr_f */
    4,	/* stxi_f */
    8,	/* bltr_f */
    20,	/* blti_f */
    12,	/* bler_f */
    24,	/* blei_f */
    8,	/* beqr_f */
    20,	/* beqi_f */
    12,	/* bger_f */
    24,	/* bgei_f */
    8,	/* bgtr_f */
    20,	/* bgti_f */
    8,	/* bner_f */
    20,	/* bnei_f */
    12,	/* bunltr_f */
    24,	/* bunlti_f */
    8,	/* bunler_f */
    20,	/* bunlei_f */
    12,	/* buneqr_f */
    24,	/* buneqi_f */
    8,	/* bunger_f */
    20,	/* bungei_f */
    12,	/* bungtr_f */
    24,	/* bungti_f */
    12,	/* bltgtr_f */
    24,	/* bltgti_f */
    8,	/* bordr_f */
    20,	/* bordi_f */
    8,	/* bunordr_f */
    20,	/* bunordi_f */
    0,	/* arg_d */
    4,	/* addr_d */
    24,	/* addi_d */
    4,	/* subr_d */
    24,	/* subi_d */
    24,	/* rsbi_d */
    4,	/* mulr_d */
    24,	/* muli_d */
    4,	/* divr_d */
    24,	/* divi_d */
    4,	/* negr_d */
    4,	/* absr_d */
    4,	/* sqrtr_d */
    12,	/* ltr_d */
    32,	/* lti_d */
    16,	/* ler_d */
    36,	/* lei_d */
    12,	/* eqr_d */
    32,	/* eqi_d */
    16,	/* ger_d */
    36,	/* gei_d */
    12,	/* gtr_d */
    32,	/* gti_d */
    16,	/* ner_d */
    36,	/* nei_d */
    16,	/* unltr_d */
    36,	/* unlti_d */
    16,	/* unler_d */
    36,	/* unlei_d */
    16,	/* uneqr_d */
    36,	/* uneqi_d */
    16,	/* unger_d */
    36,	/* ungei_d */
    16,	/* ungtr_d */
    36,	/* ungti_d */
    16,	/* ltgtr_d */
    36,	/* ltgti_d */
    16,	/* ordr_d */
    36,	/* ordi_d */
    12,	/* unordr_d */
    32,	/* unordi_d */
    12,	/* truncr_d_i */
    0,	/* truncr_d_l */
    20,	/* extr_d */
    4,	/* extr_f_d */
    4,	/* movr_d */
    24,	/* movi_d */
    4,	/* ldr_d */
    8,	/* ldi_d */
    4,	/* ldxr_d */
    4,	/* ldxi_d */
    4,	/* str_d */
    8,	/* sti_d */
    4,	/* stxr_d */
    4,	/* stxi_d */
    8,	/* bltr_d */
    28,	/* blti_d */
    12,	/* bler_d */
    32,	/* blei_d */
    8,	/* beqr_d */
    32,	/* beqi_d */
    12,	/* bger_d */
    32,	/* bgei_d */
    8,	/* bgtr_d */
    28,	/* bgti_d */
    8,	/* bner_d */
    28,	/* bnei_d */
    12,	/* bunltr_d */
    32,	/* bunlti_d */
    8,	/* bunler_d */
    28,	/* bunlei_d */
    12,	/* buneqr_d */
    32,	/* buneqi_d */
    8,	/* bunger_d */
    28,	/* bungei_d */
    12,	/* bungtr_d */
    32,	/* bungti_d */
    12,	/* bltgtr_d */
    32,	/* bltgti_d */
    8,	/* bordr_d */
    28,	/* bordi_d */
    8,	/* bunordr_d */
    28,	/* bunordi_d */
    0,	/* movr_w_f */
    0,	/* movr_ww_d */
    0,	/* movr_w_d */
    0,	/* movr_f_w */
    0,	/* movi_f_w */
    0,	/* movr_d_ww */
    0,	/* movi_d_ww */
    0,	/* movr_d_w */
    0,	/* movi_d_w */
    0,	/* x86_retval_f */
    0,	/* x86_retval_d */
    0,	/* va_start */
    0,	/* va_arg */
    0,	/* va_arg_d */
    0,	/* va_end */
#endif /* __powerpc__ */
#endif /* __WORDSIZE */

#if __WORDSIZE == 64
#if defined(__powerpc__)
#define JIT_INSTR_MAX 72
    0,	/* data */
    0,	/* live */
    4,	/* align */
    0,	/* save */
    0,	/* load */
    0,	/* #name */
    0,	/* #note */
    0,	/* label */
    72,	/* prolog */
    0,	/* arg */
    4,	/* addr */
    28,	/* addi */
    4,	/* addcr */
    28,	/* addci */
    4,	/* addxr */
    8,	/* addxi */
    4,	/* subr */
    28,	/* subi */
    4,	/* subcr */
    28,	/* subci */
    4,	/* subxr */
    8,	/* subxi */
    32,	/* rsbi */
    4,	/* mulr */
    28,	/* muli */
    12,	/* qmulr */
    28,	/* qmuli */
    12,	/* qmulr_u */
    28,	/* qmuli_u */
    4,	/* divr */
    28,	/* divi */
    4,	/* divr_u */
    28,	/* divi_u */
    20,	/* qdivr */
    16,	/* qdivi */
    20,	/* qdivr_u */
    16,	/* qdivi_u */
    12,	/* remr */
    36,	/* remi */
    12,	/* remr_u */
    36,	/* remi_u */
    4,	/* andr */
    28,	/* andi */
    4,	/* orr */
    28,	/* ori */
    4,	/* xorr */
    28,	/* xori */
    4,	/* lshr */
    4,	/* lshi */
    4,	/* rshr */
    4,	/* rshi */
    4,	/* rshr_u */
    4,	/* rshi_u */
    4,	/* negr */
    4,	/* comr */
    12,	/* ltr */
    12,	/* lti */
    12,	/* ltr_u */
    16,	/* lti_u */
    16,	/* ler */
    16,	/* lei */
    16,	/* ler_u */
    16,	/* lei_u */
    12,	/* eqr */
    12,	/* eqi */
    16,	/* ger */
    16,	/* gei */
    16,	/* ger_u */
    16,	/* gei_u */
    12,	/* gtr */
    12,	/* gti */
    12,	/* gtr_u */
    12,	/* gti_u */
    16,	/* ner */
    16,	/* nei */
    4,	/* movr */
    36,	/* movi */
    4,	/* extr_c */
    4,	/* extr_uc */
    4,	/* extr_s */
    4,	/* extr_us */
    4,	/* extr_i */
    4,	/* extr_ui */
#  if __BYTE_ORDER == __BIG_ENDIAN
    4,	/* htonr_us */
    4,	/* htonr_ui */
    4,	/* htonr_ul */
#else
    20,	/* htonr_us */
    16,	/* htonr_ui */
    44,	/* htonr_ul */
#endif
    8,	/* ldr_c */
    28,	/* ldi_c */
    4,	/* ldr_uc */
    24,	/* ldi_uc */
    4,	/* ldr_s */
    24,	/* ldi_s */
    4,	/* ldr_us */
    24,	/* ldi_us */
    4,	/* ldr_i */
    24,	/* ldi_i */
    4,	/* ldr_ui */
    24,	/* ldi_ui */
    4,	/* ldr_l */
    24,	/* ldi_l */
    8,	/* ldxr_c */
    8,	/* ldxi_c */
    4,	/* ldxr_uc */
    4,	/* ldxi_uc */
    4,	/* ldxr_s */
    4,	/* ldxi_s */
    4,	/* ldxr_us */
    4,	/* ldxi_us */
    4,	/* ldxr_i */
    4,	/* ldxi_i */
    4,	/* ldxr_ui */
    4,	/* ldxi_ui */
    4,	/* ldxr_l */
    4,	/* ldxi_l */
    4,	/* str_c */
    24,	/* sti_c */
    4,	/* str_s */
    24,	/* sti_s */
    4,	/* str_i */
    24,	/* sti_i */
    4,	/* str_l */
    24,	/* sti_l */
    4,	/* stxr_c */
    4,	/* stxi_c */
    4,	/* stxr_s */
    4,	/* stxi_s */
    4,	/* stxr_i */
    4,	/* stxi_i */
    4,	/* stxr_l */
    4,	/* stxi_l */
    8,	/* bltr */
    8,	/* blti */
    8,	/* bltr_u */
    12,	/* blti_u */
    8,	/* bler */
    8,	/* blei */
    8,	/* bler_u */
    12,	/* blei_u */
    8,	/* beqr */
    44,	/* beqi */
    8,	/* bger */
    8,	/* bgei */
    8,	/* bger_u */
    8,	/* bgei_u */
    8,	/* bgtr */
    8,	/* bgti */
    8,	/* bgtr_u */
    8,	/* bgti_u */
    8,	/* bner */
    36,	/* bnei */
    12,	/* bmsr */
    12,	/* bmsi */
    12,	/* bmcr */
    12,	/* bmci */
    12,	/* boaddr */
    16,	/* boaddi */
    12,	/* boaddr_u */
    12,	/* boaddi_u */
    12,	/* bxaddr */
    16,	/* bxaddi */
    12,	/* bxaddr_u */
    12,	/* bxaddi_u */
    12,	/* bosubr */
    16,	/* bosubi */
    12,	/* bosubr_u */
    16,	/* bosubi_u */
    12,	/* bxsubr */
    16,	/* bxsubi */
    12,	/* bxsubr_u */
    16,	/* bxsubi_u */
    0,	/* jmpr */
    4,	/* jmpi */
    28,	/* callr */
    56,	/* calli */
    72,	/* epilog */
    0,	/* arg_f */
    4,	/* addr_f */
    28,	/* addi_f */
    4,	/* subr_f */
    28,	/* subi_f */
    28,	/* rsbi_f */
    4,	/* mulr_f */
    28,	/* muli_f */
    4,	/* divr_f */
    28,	/* divi_f */
    4,	/* negr_f */
    4,	/* absr_f */
    4,	/* sqrtr_f */
    12,	/* ltr_f */
    36,	/* lti_f */
    16,	/* ler_f */
    40,	/* lei_f */
    12,	/* eqr_f */
    36,	/* eqi_f */
    16,	/* ger_f */
    40,	/* gei_f */
    12,	/* gtr_f */
    36,	/* gti_f */
    16,	/* ner_f */
    40,	/* nei_f */
    16,	/* unltr_f */
    40,	/* unlti_f */
    16,	/* unler_f */
    40,	/* unlei_f */
    16,	/* uneqr_f */
    40,	/* uneqi_f */
    16,	/* unger_f */
    40,	/* ungei_f */
    16,	/* ungtr_f */
    40,	/* ungti_f */
    16,	/* ltgtr_f */
    40,	/* ltgti_f */
    16,	/* ordr_f */
    40,	/* ordi_f */
    12,	/* unordr_f */
    36,	/* unordi_f */
    12,	/* truncr_f_i */
    12,	/* truncr_f_l */
    12,	/* extr_f */
    4,	/* extr_d_f */
    4,	/* movr_f */
    24,	/* movi_f */
    4,	/* ldr_f */
    24,	/* ldi_f */
    4,	/* ldxr_f */
    4,	/* ldxi_f */
    4,	/* str_f */
    24,	/* sti_f */
    4,	/* stxr_f */
    4,	/* stxi_f */
    8,	/* bltr_f */
    32,	/* blti_f */
    12,	/* bler_f */
    36,	/* blei_f */
    8,	/* beqr_f */
    32,	/* beqi_f */
    12,	/* bger_f */
    36,	/* bgei_f */
    8,	/* bgtr_f */
    32,	/* bgti_f */
    8,	/* bner_f */
    32,	/* bnei_f */
    12,	/* bunltr_f */
    36,	/* bunlti_f */
    8,	/* bunler_f */
    32,	/* bunlei_f */
    12,	/* buneqr_f */
    36,	/* buneqi_f */
    8,	/* bunger_f */
    32,	/* bungei_f */
    12,	/* bungtr_f */
    36,	/* bungti_f */
    12,	/* bltgtr_f */
    36,	/* bltgti_f */
    8,	/* bordr_f */
    32,	/* bordi_f */
    8,	/* bunordr_f */
    32,	/* bunordi_f */
    0,	/* arg_d */
    4,	/* addr_d */
    28,	/* addi_d */
    4,	/* subr_d */
    28,	/* subi_d */
    28,	/* rsbi_d */
    4,	/* mulr_d */
    28,	/* muli_d */
    4,	/* divr_d */
    28,	/* divi_d */
    4,	/* negr_d */
    4,	/* absr_d */
    4,	/* sqrtr_d */
    12,	/* ltr_d */
    40,	/* lti_d */
    16,	/* ler_d */
    44,	/* lei_d */
    12,	/* eqr_d */
    40,	/* eqi_d */
    16,	/* ger_d */
    44,	/* gei_d */
    12,	/* gtr_d */
    40,	/* gti_d */
    16,	/* ner_d */
    44,	/* nei_d */
    16,	/* unltr_d */
    44,	/* unlti_d */
    16,	/* unler_d */
    44,	/* unlei_d */
    16,	/* uneqr_d */
    44,	/* uneqi_d */
    16,	/* unger_d */
    44,	/* ungei_d */
    16,	/* ungtr_d */
    44,	/* ungti_d */
    16,	/* ltgtr_d */
    44,	/* ltgti_d */
    16,	/* ordr_d */
    44,	/* ordi_d */
    12,	/* unordr_d */
    40,	/* unordi_d */
    12,	/* truncr_d_i */
    12,	/* truncr_d_l */
    12,	/* extr_d */
    4,	/* extr_f_d */
    4,	/* movr_d */
    32,	/* movi_d */
    4,	/* ldr_d */
    24,	/* ldi_d */
    4,	/* ldxr_d */
    4,	/* ldxi_d */
    4,	/* str_d */
    24,	/* sti_d */
    4,	/* stxr_d */
    4,	/* stxi_d */
    8,	/* bltr_d */
    32,	/* blti_d */
    12,	/* bler_d */
    36,	/* blei_d */
    8,	/* beqr_d */
    40,	/* beqi_d */
    12,	/* bger_d */
    40,	/* bgei_d */
    8,	/* bgtr_d */
    36,	/* bgti_d */
    8,	/* bner_d */
    36,	/* bnei_d */
    12,	/* bunltr_d */
    36,	/* bunlti_d */
    8,	/* bunler_d */
    32,	/* bunlei_d */
    12,	/* buneqr_d */
    36,	/* buneqi_d */
    8,	/* bunger_d */
    36,	/* bungei_d */
    12,	/* bungtr_d */
    40,	/* bungti_d */
    12,	/* bltgtr_d */
    40,	/* bltgti_d */
    8,	/* bordr_d */
    36,	/* bordi_d */
    8,	/* bunordr_d */
    32,	/* bunordi_d */
    0,	/* movr_w_f */
    0,	/* movr_ww_d */
    0,	/* movr_w_d */
    0,	/* movr_f_w */
    0,	/* movi_f_w */
    0,	/* movr_d_ww */
    0,	/* movi_d_ww */
    0,	/* movr_d_w */
    0,	/* movi_d_w */
    0,	/* x86_retval_f */
    0,	/* x86_retval_d */
    0,	/* va_start */
    0,	/* va_arg */
    0,	/* va_arg_d */
    0,	/* va_end */
#endif /* __powerpc__ */
#endif /* __WORDSIZE */
