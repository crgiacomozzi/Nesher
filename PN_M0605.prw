#INCLUDE 'Totvs.ch'
#INCLUDE 'FWMVCDef.ch'
#INCLUDE "TBIConn.ch"

//Variáveis Estáticas
Static cTitulo := "Compensação entre carteiras Koncili"
Static cAlias  := "ZZA"

/*/{Protheus.doc} M0604
    Função para montagem da tela de compensação entre carteiras de títulos da Koncili
    @type  Function
    @author Pono Tecnologia
    @since 22/07/2022
    @version 12.1.33
    /*/
User Function M0605()
    Local aArea   	:= GetArea()
    Local oBrowse
    Private lRpc    := Type("cFilAnt") == "U"
    Private aRotina := MenuDef()
    Private lInverte
    Private aCarrega:= {}
    Private aImprime:= {}
    Private aErros  := {}

    If lRpc
		RPCSetType(3)
		RpcSetEnv('01','02',,,,GetEnvServer(),{ })
	Else 
        If cFilAnt == "01"
            Aviso("Atenção!","Operação não permitida para a MATRIZ.",{"Ok"})
            Return
        EndIf
    EndIf

    SetKey (VK_F12,{|a,b| AcessaPerg("M0605",.T.)})

    oBrowse := FWMBrowse():New()
    oBrowse:SetAlias(cAlias)
    oBrowse:SetDescription(cTitulo)
    oBrowse:Activate()
     
    RestArea(aArea)

Return Nil
 
/*/{Protheus.doc} MenuDef
    Criação do menu MVC
    @type Function
    @author Pono Tecnologia
    @since 22/07/2022
    @version 12.1.33
    /*/
Static Function MenuDef()
    Local aRot := {}
     
    //Adicionando opções
    //ADD OPTION aRot TITLE 'Compensar Títulos'   ACTION 'U_M0606(MV_PAR01,MV_PAR02,ZZA_MKTID,ZZA_CODFOR,ZZA_LOJFOR)'  OPERATION 0 ACCESS 0
    ADD OPTION aRot TITLE 'Compensar Títulos'   ACTION 'U_M0605A(ZZA_CODMKT,ZZA_CODFOR,ZZA_LOJFOR)'  OPERATION 0 ACCESS 0
 
Return aRot
 
/*/{Protheus.doc} ModelDef
    Criação do modelo de dados MVC.
    @type Function
    @author Pono Tecnologia
    @since 22/07/2022
    @version 12.1.33
    /*/
Static Function ModelDef()
    Local oModel := Nil
    Local oMaster := FWFormStruct(1, cAlias)
     
    oModel := MPFormModel():New('PN_M0605M') 
     
    oModel:AddFields("MASTER",/*cOwner*/,oMaster)
     
    //Setando a chave primária da rotina
    oModel:SetPrimaryKey({"ZZA_FILIAL", "ZZA_MKTID" })
     
    //Adicionando descrição ao modelo
    oModel:SetDescription(cTitulo)
     
    //Setando a descrição do formulário
    oModel:GetModel("MASTER"):SetDescription("Formulário do Cadastro "+cTitulo)
Return oModel
 
/*/{Protheus.doc} ViewDef
    Criação da visão MVC.
    @type Function
    @author Pono Tecnologia
    @since 22/07/2022
    @version 12.1.33
    /*/
Static Function ViewDef()
    Local oModel := FWLoadModel("PN_M0605")
    Local oMaster := FWFormStruct(2, cAlias)  //pode se usar um terceiro parâmetro para filtrar os campos exibidos { |cCampo| cCampo $ 'SBM_NOME|SBM_DTAFAL|'}
    Local oView := Nil
 
    oView := FWFormView():New()
    oView:SetModel(oModel)
    oView:AddField("VIEW", oMaster, "MASTER")
    oView:CreateHorizontalBox("TELA",100)
    oView:EnableTitleView('VIEW', 'Cadastro de Parada' )  
    oView:SetCloseOnOk({||.T.})
    oView:SetOwnerView("VIEW","TELA")

Return oView

User Function M0605A(cMarket,cFornece,cLojFor)
    Local oDlg
    Local aSize   := {}
    Local aBut450 := {}
    Local oTotalP
    Local oTotalR
    Local oSelecP
    Local oSelecR
    Local nTotalP := 0
    Local nTotalR := 0
    Local nSelecP := 0
    Local nSelecR := 0
    Local oFnt
    Local cMarca
    Local TRB2     := ""
    Local aCampos := {  {"TMP_OK"       ,"C", 2,0},;
                        {"TMP_TIPO"     ,"C", 1,0},;
                        {"TMP_TITULO"   ,"C", 9,0},;
                        {"TMP_PREFIX"   ,"C", 3,0},;
                        {"TMP_PARCEL"   ,"C", 1,0},;
                        {"TMP_TPTIT"    ,"C", 3,0},;
                        {"TMP_VLPAG"    ,"N",14,2},;
                        {"TMP_VLREC"    ,"N",14,2},;
                        {"TMP_EMISSA"   ,"D", 8,0},;
                        {"TMP_VENCTO"   ,"D", 8,0},;
                        {"TMP_SALDO"	,"N",14,2},;						
                        {"TMP_JUROS"    ,"N",14,2},;
                        {"TMP_MULTA"    ,"N",14,2},;
                        {"TMP_DESCON"   ,"N",14,2},;
                        {"TMP_ACRESC"	,"N",14,2},;
                        {"TMP_DESCRE"   ,"N",14,2},;
                        {"TMP_CLIFOR"   ,"C", 6,0},;
                        {"TMP_LOJA"	    ,"C", 2,0},;
                        {"TMP_NOME"     ,"C",30,0}}

    Local aCpoBro	:= {{"TMP_OK"	    ,, " "                  ,"  "},;
                        {"TMP_TIPO"     ,, "Carteira"           ,"!"},;
                        {"TMP_TITULO"	,, "Número Título"      ,"@X"},;
                        {"TMP_PREFIX"	,, "Prefixo"            ,"@X"},;
                        {"TMP_PARCEL"	,, "Parcela"            ,"@X"},;
                        {"TMP_TPTIT"	,, "Tipo do Título"     ,"@X"},;
                        {"TMP_VLPAG"	,, "Valor Pagar"        ,"@E 999,999,999.99"},;
                        {"TMP_VLREC"	,, "Valor Receber"      ,"@E 999,999,999.99"},;
                        {"TMP_EMISSA"	,, "Data EmissÆo"       ,"@X"},;
                        {"TMP_VENCTO"	,, "Data Vencimento"    ,"@X"},;
                        {"TMP_SALDO"	,, "Saldo Titulo"       ,"@E 9,999,999,999.99"},;
                        {"TMP_JUROS"	,, "Juros"              ,"@E 9,999,999,999.99"},;
                        {"TMP_MULTA"	,, "Multa"              ,"@E 9,999,999,999.99"},;
                        {"TMP_DESCON"	,, "Descontos"          ,"@E 9,999,999,999.99"},;
                        {"TMP_ACRESC"	,, "Acrescimos"         ,"@E 9,999,999,999.99"},;
                        {"TMP_DESCRE"	,, "Decrescimos"        ,"@E 9,999,999,999.99"},;
                        {"TMP_CLIFOR"	,, "Cli/For"            ,"@X"},;
                        {"TMP_LOJA"	    ,, "Loja"               ,"@X"},;
                        {"TMP_NOME"		,, "Nome"               ,"@X"}}

    Pergunte("M0605",.T.)

    dDtIni := MV_PAR01
    dDtFim := MV_PAR02
    nLimite:= MV_PAR03

    If Select("TRB2") > 0
        dbSelectArea("TRB2")
        dbCloseArea()
    EndIf

    aCarrega := {}

    TRB2 := U_M0605B(aCampos,dDtIni,dDtFim,cMarket,cFornece,cLojFor,@nTotalP,@nTotalR)

    DbSelectArea("TRB2")
    TRB2->(DbGoTop())

    DEFINE FONT oFnt NAME "Arial" SIZE 12,14 BOLD

    aSize := MSADVSIZE()	

    DEFINE MSDIALOG oDlg TITLE "Compensação Entre Carteiras" From aSize[7],0 To aSize[6],aSize[5] OF oMainWnd PIXEL
    oDlg:lMaximized := .T.

    /*
    oPanel := TPanel():New(0,0,'',oDlg,, .T., .T.,, ,20,20,.T.,.T. )
    oPanel:Align := CONTROL_ALIGN_TOP

    @003, 005 Say "Compensacao Nr." 	FONT oDlg:oFont PIXEL OF oPanel
    @003, 060 Say cNumComp Picture "@!"	FONT oFnt COLOR CLR_HBLUE PIXEL OF oPanel
    */

    oMark := MsSelect():New("TRB2","TMP_OK","",aCpoBro,@lInverte,@cMarca,{50,oDlg:nLeft,oDlg:nBottom,oDlg:nRight})
    oMark:bMark := {| | U_M0605D(cMarca,lInverte,@nSelecP,@NSelecR,oSelecP,oSelecR)}
    oMark:oBrowse:lhasMark = .t.
    oMark:oBrowse:lCanAllmark := .t.
    //oMark:bAval	:= {|| U_M0605D(cMarca,lInverte,@nTotalP,@nTotalR,@nSelecP,@nSelecR),oMark:oBrowse:Refresh(.T.) }
                                
    oMark:oBrowse:bAllMark := { || U_M0605G(cMarca,lInverte,@nSelecP,@NSelecR,oSelecP,oSelecR),oMark:oBrowse:Refresh(.T.)}
    oMark:oBrowse:Align := CONTROL_ALIGN_ALLCLIENT

    oPanel2 := TPanel():New(0,0,'',oDlg,, .T., .T.,, ,40,40,.T.,.T. )
    
    oPanel2:Align := CONTROL_ALIGN_BOTTOM

    @003,060	Say "Pagar"   FONT oDlg:oFont PIXEL OF oPanel2
    @003,200	Say "Receber" FONT oDlg:oFont PIXEL OF oPanel2

    @012,005	Say "Total Exibido :" FONT oDlg:oFont PIXEL OF oPanel2
    @012,060 Say oTotalP VAR nTotalP 	Picture "@E 999,999,999,999,999.99" FONT oDlg:oFont PIXEL OF oPanel2
    @012,200 Say oTotalR VAR nTotalR 	Picture "@E 999,999,999,999,999.99" FONT oDlg:oFont PIXEL OF oPanel2
    
    @021,005 Say "Total Selecionado :" FONT oDlg:oFont PIXEL OF oPanel2
    @021,060 Say oSelecP VAR nSelecP 	Picture "@E 999,999,999,999,999.99" FONT oDlg:oFont PIXEL OF oPanel2
    @021,200 Say oSelecR VAR nSelecR 	Picture "@E 999,999,999,999,999.99" FONT oDlg:oFont PIXEL OF oPanel2
    // Panel

    ACTIVATE MSDIALOG oDlg ON INIT EnchoiceBar(oDlg,{|| U_M0605I(dDtIni,dDtFim,nLimite,cFornece,cLojFor),oDlg:End()},{|| oDlg:End()},,aBut450)
    

Return

User Function M0605B(aCampos,dDtIni,dDtFim,cMarket,cFornece,cLojFor,nTotalP,nTotalR)
    Local nX

    If(Type('oTable') <> 'U')
        oTable:Delete()
        oTable := Nil
    EndIf

    oTable := FwTemporaryTable():New('TRB2')
    oTable:SetFields(aCampos)

    oTable:Create()

    U_M0605E(@aCarrega,dDtIni,dDtFim,cFornece,cLojFor,@nTotalP) // carrega títulos a pagar
    U_M0605F(@aCarrega,dDtIni,dDtFim,cMarket,@nTotalR) // carrega títulos a receber

    If Len(aCarrega) > 0

        For nX := 1 To Len(aCarrega)

            DbSelectArea('TRB2')

            RecLock('TRB2', .T.)
        
                TRB2->TMP_TIPO      := aCarrega[nX,1]
                TRB2->TMP_TITULO    := aCarrega[nX,2]
                TRB2->TMP_PREFIX    := aCarrega[nX,16]
                TRB2->TMP_PARCEL    := aCarrega[nX,17]
                TRB2->TMP_TPTIT     := aCarrega[nX,18]
                TRB2->TMP_VLPAG     := aCarrega[nX,3]
                TRB2->TMP_VLREC     := aCarrega[nX,4]
                TRB2->TMP_EMISSA    := STOD(aCarrega[nX,5])
                TRB2->TMP_VENCTO    := STOD(aCarrega[nX,6])
                TRB2->TMP_SALDO     := aCarrega[nX,7]
                TRB2->TMP_JUROS     := aCarrega[nX,8]
                TRB2->TMP_MULTA     := aCarrega[nX,9]
                TRB2->TMP_DESCON    := aCarrega[nX,10]
                TRB2->TMP_ACRESC    := aCarrega[nX,11]
                TRB2->TMP_DESCRE    := aCarrega[nX,12]
                TRB2->TMP_CLIFOR    := aCarrega[nX,13]
                TRB2->TMP_LOJA      := aCarrega[nX,14]
                TRB2->TMP_NOME      := aCarrega[nX,15]

            TRB2->(MsUnlock())
        
        Next nX

        TRB2->(DbGoTop())

    EndIf

Return

/*/{Protheus.doc} M0605C
    Função que realiza o MSExecAuto da compensação de títulos
    @type  Function
    @author Pono Tecnologia
    @since 08/08/2022
    @version 12.1.33
    /*/
User Function M0605C(dDtIni,dDtFim,nLimite,cFornece,cLojFor)
    Local aArea             := GetArea()
    Local lRet              := .T.
    Local nI, nX
    Local aErro             := {}
    Local nOpcao            := 3
    Local aSE1Rec           := {}
    Local aSE2Pag           := {}
    Local aCliente          := {}
    Private lMsErroAuto     := .F.
    Private lAutoErrNoFile  := .T. 

    TRB2->(dbGoTop())
    
    While !TRB2->(Eof())
        If !Empty(TRB2->TMP_OK)
            If TRB2->TMP_TIPO == "P"
                AAdd(aSE2Pag, {xFilial("SE2")+TRB2->TMP_PREFIX+TRB2->TMP_TITULO+TRB2->TMP_PARCEL+TRB2->TMP_TPTIT+TRB2->TMP_CLIFOR+TRB2->TMP_LOJA})
            ElseIf TRB2->TMP_TIPO == "R"
                AADD(aCliente, { TRB2->TMP_CLIFOR,TRB2->TMP_LOJA })
                AAdd(aSE1Rec, {xFilial("SE1")+TRB2->TMP_PREFIX+TRB2->TMP_TITULO+TRB2->TMP_PARCEL+TRB2->TMP_TPTIT})
            EndIf
        EndIf
        TRB2->(dbSkip())
    EndDo

    If nOpcao == 3
        For nX := 1 To Len(aCliente)
            aAutoCab := {   {"AUTDVENINI450", dDtIni          , nil},;
                            {"AUTDVENFIM450", dDtFim          , nil},;
                            {"AUTNLIM450"   , nLimite         , nil},;
                            {"AUTCCLI450"   , aCliente[nX,1]  , nil},;
                            {"AUTCLJCLI"    , aCliente[nX,2]  , nil},;
                            {"AUTCFOR450"   , cFornece        , nil},;
                            {"AUTCLJFOR"    , cLojFor         , nil},;
                            {"AUTCMOEDA450" , "01"            , nil},;
                            {"AUTNDEBCRED"  , 1               , nil},;
                            {"AUTLTITFUTURO", .F.             , nil},;
                            {"AUTARECCHAVE" , aSE1Rec         , nil},;
                            {"AUTAPAGCHAVE" , aSE2Pag         , nil}}
        
            MSExecAuto({|x,y,z| Fina450(x,y,z)}, nil , aAutoCab , 3 )
            If lMsErroAuto
                lRet := .F.
                cMsg := "Problema : " + CRLF
                aErro := GetAutoGRLog()
                For nI := 1 To Len(aErro)
                    cMsg += aErro[nI] + CRLF
                Next nI
            EndIf
        Next nX
    EndIf

    If Len(aErro) > 0
        U_M0605H(aErro)
    Else 
        Aviso( "Compensação entre carteiras", "Compensação entre carteiras realizada com sucesso", { "Sim"} , 1)
    EndIf


    RestArea(aArea)

Return(lRet)

User Function M0605D(cMarca,lInverte,nSelecP,nSelecR,oSelecP,oSelecR)

    RecLock("TRB2",.F.)

        If Marked("TMP_OK")
            TRB2->TMP_OK := cMarca
            If TRB2->TMP_VLREC != 0
                nSelecR += TRB2->TMP_VLREC
            Else
                nSelecP += TRB2->TMP_VLPAG
            EndIf
        Else
            TRB2->TMP_OK := " "
            If TRB2->TMP_VLREC != 0
                nSelecR -= TRB2->TMP_VLREC
            Else
                nSelecP -= TRB2->TMP_VLPAG
            EndIf
        End

    MsUnlock()

    oSelecP:Refresh()
    oSelecR:Refresh()

Return

/*/{Protheus.doc} M0605E
    Função que busca os títulos a pagar para compensação.
    @type  Function
    @author user
    @author Pono Tecnologia
    @since 08/08/2022
    /*/
User Function M0605E(aCarrega,dDtIni,dDtFim,cFornece,cLojFor,nTotalP)
    Local cAlias := GetNextAlias()
    Local cQuery

    cQuery := "SELECT * "
    cQuery += "FROM " + RetSqlName("SE2") + " "
    cQuery += "WHERE D_E_L_E_T_ = ' ' "
    cQuery += "AND E2_FILIAL = '" + xFilial("SE2") + "' "
    cQuery += "AND E2_FORNECE = '" + Alltrim(cFornece) + "' "
    cQuery += "AND E2_LOJA = '" + Alltrim(cLojFor) + "' "
    cQuery += "AND E2_SALDO > 0 "
    cQuery += "AND E2_EMISSAO >= '" + DTOS(dDtIni) + "' AND E2_EMISSAO <= '" + DTOS(dDtFim) + "'"

    MPSysOpenQuery( cQuery, cAlias )

    DBSelectArea(cAlias)
    (cAlias)->(dbGoTop())
    
    While !(cAlias)->((Eof()))
        AADD( aCarrega, { "P", (cAlias)->E2_NUM, (cAlias)->E2_SALDO, 0, (cAlias)->E2_EMISSAO, (cAlias)->E2_VENCTO, (cAlias)->E2_SALDO, (cAlias)->E2_JUROS, (cAlias)->E2_MULTA, (cAlias)->E2_DESCONT, (cAlias)->E2_ACRESC, (cAlias)->E2_DECRESC, (cAlias)->E2_FORNECE, (cAlias)->E2_LOJA, (cAlias)->E2_NOMFOR, (cAlias)->E2_PREFIXO, (cAlias)->E2_PARCELA, (cAlias)->E2_TIPO } )
        nTotalP += (cAlias)->E2_SALDO
        (cAlias)->(dbSkip())
    EndDo
    
    (cAlias)->(dbCloseArea())    

Return

/*/{Protheus.doc} M0605F
    Função que busca os títulos a receber para compensação.
    @type  Function
    @author user
    @author Pono Tecnologia
    @since 08/08/2022
    /*/
User Function M0605F(aCarrega,dDtIni,dDtFim,cMarket,nTotalR)
    Local cAlias   := GetNextAlias()
    Local cQuery

    cQuery := "SELECT * "
    cQuery += "FROM " + RetSqlName("SE1") + " "
    cQuery += "WHERE D_E_L_E_T_ = ' ' "
    cQuery += "AND E1_FILIAL = '" + xFilial("SE1") + "' "
    cQuery += "AND E1_NUMLIQ != '' "
    cQuery += "AND E1_SALDO > 0 "
    cQuery += "AND E1_EMISSAO >= '" + DTOS(dDtIni) + "' AND E1_EMISSAO <= '" + DTOS(dDtFim) + "' "
    cQuery += "AND E1_CODMKT = '" + cMarket + "' "
    cQuery += "AND E1_RESOLKO = 'S'"

    MPSysOpenQuery( cQuery, cAlias )

    DBSelectArea(cAlias)
    (cAlias)->(dbGoTop())
    
    While !(cAlias)->((Eof()))
        AADD( aCarrega, { "R", (cAlias)->E1_NUM, 0, (cAlias)->E1_SALDO, (cAlias)->E1_EMISSAO, (cAlias)->E1_VENCTO, (cAlias)->E1_SALDO, (cAlias)->E1_JUROS, (cAlias)->E1_MULTA, (cAlias)->E1_DESCONT, (cAlias)->E1_ACRESC, (cAlias)->E1_DECRESC, (cAlias)->E1_CLIENTE, (cAlias)->E1_LOJA, (cAlias)->E1_NOMCLI, (cAlias)->E1_PREFIXO, (cAlias)->E1_PARCELA, (cAlias)->E1_TIPO } )
        nTotalR += (cAlias)->E1_SALDO
        (cAlias)->(dbSkip())
    EndDo
    
    (cAlias)->(dbCloseArea())    

Return(aCarrega)

User Function M0605G(cMarca,lInverte,nSelecP,nSelecR,oSelecP,oSelecR)

    TRB2->(dbGoTop())

    While TRB2->(!Eof())
        RecLock("TRB2",.F.)
            If !Marked("TMP_OK")
                TRB2->TMP_OK := cMarca
                If TRB2->TMP_VLREC != 0
                    nSelecR += TRB2->TMP_VLREC
                Else
                    nSelecP += TRB2->TMP_VLPAG
                EndIf
            Else
                TRB2->TMP_OK := " "
                If TRB2->TMP_VLREC != 0
                    nSelecR -= TRB2->TMP_VLREC
                Else
                    nSelecP -= TRB2->TMP_VLPAG
                EndIf
            End
        MsUnlock()
    	TRB2->(DbSkip())	
    Enddo

    TRB2->(dbGoTop())

    oSelecP:Refresh()
    oSelecR:Refresh()

Return

/*/{Protheus.doc} M0605H
    Função que exibe log de erros quando rotina de baixa de títulos Precode é executada a partir do menu
    @type  Function
    @author Pono
    @since 08/07/2022
    @version 12.1.33
    @param aErros, array, array contendo as mensagens dos erros encontrados durante o processamento
    /*/
User Function M0605H(aErros)
    Local oFntTxt := TFont():New("Calibri",,-14,,.F.,,,,,.F.,.F.)
    Local cTitulo := "Log de erro"
    Local cMsg    := ""
    Local nX

    For nX := 1 To Len(aErros)
        cData := SubStr(aErros[nX,1],9,2) + "/" + SubStr(aErros[nX,1],6,2) + "/" + SubStr(aErros[nX,1],1,4)
        cMsg += "Problema : " + aErros[nX,3] + CRLF
        cMsg += + CRLF
    Next nX

    DEFINE MSDIALOG oDlgMens TITLE cTitulo FROM 000, 000  TO 300, 600 COLORS 0, 16777215 PIXEL
        @ 002, 004 GET oMsg VAR cMsg OF oDlgMens MULTILINE SIZE 391, 121 FONT oFntTxt COLORS 0, 16777215 HSCROLL PIXEL
        @ 127, 134 BUTTON oBtnOk  PROMPT "&Ok" SIZE 051, 019 ACTION oDlgMens:End() OF oDlgMens PIXEL
    ACTIVATE MSDIALOG oDlgMens CENTERED

Return

User Function M0605I(dDtIni,dDtFim,nLimite,cFornece,cLojFor)

    FwMsgRun(,{ || U_M0605C(dDtIni,dDtFim,nLimite,cFornece,cLojFor) }, "Compensação de títulos", 'Processando dados...')

Return
