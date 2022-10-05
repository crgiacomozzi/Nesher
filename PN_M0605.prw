#INCLUDE 'Totvs.ch'
#INCLUDE 'FWMVCDef.ch'
#INCLUDE "TBIConn.ch"

//Variáveis Estáticas
Static cTitulo := "Compensação entre carteiras Koncili"
Static cAlias  := "ZZA"

/*/{Protheus.doc} M0605
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
     
    ADD OPTION aRot TITLE 'Compensar Títulos'     ACTION 'U_M0605A(ZZA_CODMKT,ZZA_MKTID)'  OPERATION 0 ACCESS 0
    ADD OPTION aRot TITLE 'Cancelar Compensação'  ACTION 'U_M0605J(ZZA_CODMKT,ZZA_MKTID)'  OPERATION 0 ACCESS 0
 
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
    Local oMaster := FWFormStruct(2, cAlias)
    Local oView := Nil
 
    oView := FWFormView():New()
    oView:SetModel(oModel)
    oView:AddField("VIEW", oMaster, "MASTER")
    oView:CreateHorizontalBox("TELA",100)
    oView:EnableTitleView('VIEW', 'Compensação' )  
    oView:SetCloseOnOk({||.T.})
    oView:SetOwnerView("VIEW","TELA")

Return oView

/*/{Protheus.doc} M0605A
    Função responsável pela montagem da tela de compensaçao de títulos entre carterias customizada.
    @type Function
    @author Pono Tecnologia
    @since 22/07/2022
    @version 12.1.33
    @param cMarket, Character, Código do marketplace
    @param cMktId , Character, Id do marketplace
    /*/
User Function M0605A(cMarket,cMktId)
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
    Local nOpcao   := 3
    Local aFornece := {}
    Local cMsg     := ""
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

    aFornece := U_M0605N(cMktId)
    
    If Len(aFornece) > 0
        TRB2 := U_M0605B(aCampos,dDtIni,dDtFim,cMarket,aFornece,@nTotalP,@nTotalR)
    Else 
        Aviso("Atenção!","Amarração fornecedor X marketplace não encontrada.",{"Ok"})
        Return
    EndIf

    DbSelectArea("TRB2")
    TRB2->(DbGoTop())

    DEFINE FONT oFnt NAME "Arial" SIZE 12,14 BOLD

    aSize := MSADVSIZE()	

    DEFINE MSDIALOG oDlg TITLE "Compensação Entre Carteiras" From aSize[7],0 To aSize[6],aSize[5] OF oMainWnd PIXEL
    oDlg:lMaximized := .T.

    oMark := MsSelect():New("TRB2","TMP_OK","",aCpoBro,@lInverte,@cMarca,{50,oDlg:nLeft,oDlg:nBottom,oDlg:nRight})
    oMark:bMark := {| | U_M0605D("TRB2",cMarca,lInverte,@nSelecP,@NSelecR,oSelecP,oSelecR)}
    oMark:oBrowse:lhasMark = .t.
    oMark:oBrowse:lCanAllmark := .t.
                                
    oMark:oBrowse:bAllMark := { || U_M0605G("TRB2",cMarca,lInverte,@nSelecP,@NSelecR,oSelecP,oSelecR),oMark:oBrowse:Refresh(.T.)}
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

    ACTIVATE MSDIALOG oDlg ON INIT EnchoiceBar(oDlg,{|| IIF(U_M0605I("TRB2",dDtIni,dDtFim,nLimite,nOpcao,@cMsg),oDlg:End(),Aviso("Atenção!",cMsg,{"Ok"})) },{|| oDlg:End()},,aBut450)
    
Return


/*/{Protheus.doc} M0605B
    Função responsável pela montegem dos arrays que serão utilizados na tela, informações dos títulos a pagar e receber.
    @type  Function
    @author Pono Tecnologia
    @since 22/07/2022
    @version 12.1.33
    /*/
User Function M0605B(aCampos,dDtIni,dDtFim,cMarket,aFornece,nTotalP,nTotalR)
    Local nX

    If(Type('oTable') <> 'U')
        oTable:Delete()
        oTable := Nil
    EndIf

    oTable := FwTemporaryTable():New('TRB2')
    oTable:SetFields(aCampos)

    oTable:Create()

    U_M0605E(@aCarrega,dDtIni,dDtFim,aFornece,@nTotalP) // carrega títulos a pagar
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
User Function M0605C(cAlias,dDtIni,dDtFim,nLimite,nOpcao)
    Local aArea             := GetArea()
    Local lRet              := .T.
    Local nI, nX
    Local aErro             := {}
    Local aSE1Rec           := {}
    Local aSE2Pag           := {}
    Local aCliente          := {}
    Private lMsErroAuto     := .F.
    Private lAutoErrNoFile  := .T. 

    (cAlias)->(dbGoTop())
    
    While !(cAlias)->(Eof())
        If !Empty((cAlias)->TMP_OK)
            If (cAlias)->TMP_TIPO == "P"
                AAdd(aSE2Pag, {xFilial("SE2")+(cAlias)->TMP_PREFIX+(cAlias)->TMP_TITULO+(cAlias)->TMP_PARCEL+(cAlias)->TMP_TPTIT+(cAlias)->TMP_CLIFOR+(cAlias)->TMP_LOJA})
                cFornece := (cAlias)->TMP_CLIFOR
                cLojFor  := (cAlias)->TMP_LOJA
            ElseIf (cAlias)->TMP_TIPO == "R"
                AADD(aCliente, { (cAlias)->TMP_CLIFOR,(cAlias)->TMP_LOJA })
                AAdd(aSE1Rec, {xFilial("SE1")+(cAlias)->TMP_PREFIX+(cAlias)->TMP_TITULO+(cAlias)->TMP_PARCEL+(cAlias)->TMP_TPTIT})
            EndIf
        EndIf
        (cAlias)->(dbSkip())
    EndDo

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
    
        MSExecAuto({|x,y,z| Fina450(x,y,z)}, nil , aAutoCab , nOpcao )
        If lMsErroAuto
            cMsg := ""
            lRet := .F.
            cMsg := "Cliente : " + aCliente[nX,1] + " - " + aCliente[nX,2] + CRLF
            cMsg += "Problema : " + CRLF
            aErro := GetAutoGRLog()
            For nI := 1 To Len(aErro)
                cMsg += aErro[nI] + CRLF
            Next nI
            AADD( aErros, { cMsg } )
        EndIf
    Next nX

    If Len(aErros) > 0
        U_M0605H(aErros)
    Else 
        If nOpcao == 3
            Aviso( "Compensação entre carteiras", "Compensação entre carteiras realizada com sucesso", { "Sim"} , 1)
        ElseIf nOpcao == 5
            Aviso( "Cancelamento de Compensação entre carteiras", "Cancelmanento de compensação entre carteiras realizada com sucesso", { "Sim"} , 1)
        EndIf
    EndIf

    RestArea(aArea)

Return(lRet)

/*/{Protheus.doc} M0605D
    Função responsável pela validação da marcação e pela atualização das variáveis de totais da tela na rotina de compensação.
    @type  Function
    @author Pono Tecnologia
    @since 22/07/2022
    @version 12.1.33+
    /*/
User Function M0605D(cAlias,cMarca,lInverte,nSelecP,nSelecR,oSelecP,oSelecR)

    RecLock(cAlias,.F.)

        If Marked("TMP_OK")
            (cAlias)->TMP_OK := cMarca
            If (cAlias)->TMP_VLREC != 0
                nSelecR += (cAlias)->TMP_VLREC
            Else
                nSelecP += (cAlias)->TMP_VLPAG
            EndIf
        Else
            (cAlias)->TMP_OK := " "
            If (cAlias)->TMP_VLREC != 0
                nSelecR -= (cAlias)->TMP_VLREC
            Else
                nSelecP -= (cAlias)->TMP_VLPAG
            EndIf
        End

    MsUnlock()

    oSelecP:Refresh()
    oSelecR:Refresh()

Return

/*/{Protheus.doc} M0605E
    Função que busca os títulos a pagar para compensação.
    @type  Function
    @author Pono Tecnologia
    @since 08/08/2022
    @version 12.1.33
    /*/
User Function M0605E(aCarrega,dDtIni,dDtFim,aFornece,nTotalP)
    Local cAlias := GetNextAlias()
    Local cQuery
    Local nX

    For nX := 1 to Len(aFornece)
        cQuery := "SELECT * "
        cQuery += "FROM " + RetSqlName("SE2") + " "
        cQuery += "WHERE D_E_L_E_T_ = ' ' "
        cQuery += "AND E2_FILIAL = '" + xFilial("SE2") + "' "
        cQuery += "AND E2_FORNECE = '" + Alltrim(aFornece[nX,1]) + "' "
        cQuery += "AND E2_LOJA = '" + Alltrim(aFornece[nX,2]) + "' "
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
    Next nX

Return

/*/{Protheus.doc} M0605F
    Função que busca os títulos a receber para compensação.
    @type  Function
    @author Pono Tecnologia
    @since 08/08/2022
    @version 12.1.33
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

/*/{Protheus.doc} M0605G
    Função responsável pela atualização dos totalizadores conforme os títulos selecionados.
    @type  Function
    @author Pono Tecnologia
    @since 08/08/2022
    @version 12.1.33
    /*/
User Function M0605G(cAlias,cMarca,lInverte,nSelecP,nSelecR,oSelecP,oSelecR)

    (cAlias)->(dbGoTop())

    While (cAlias)->(!Eof())
        RecLock(cAlias,.F.)
            If !Marked("TMP_OK")
                (cAlias)->TMP_OK := cMarca
                If (cAlias)->TMP_VLREC != 0
                    nSelecR += (cAlias)->TMP_VLREC
                Else
                    nSelecP += (cAlias)->TMP_VLPAG
                EndIf
            Else
                (cAlias)->TMP_OK := " "
                If (cAlias)->TMP_VLREC != 0
                    nSelecR -= (cAlias)->TMP_VLREC
                Else
                    nSelecP -= (cAlias)->TMP_VLPAG
                EndIf
            End
        MsUnlock()
    	(cAlias)->(DbSkip())
    Enddo

    (cAlias)->(dbGoTop())

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
        cMsg += "Problema : " + aErros[nX,1] + CRLF
        cMsg += " " + CRLF
    Next nX

    DEFINE MSDIALOG oDlgMens TITLE cTitulo FROM 000, 000  TO 300, 600 COLORS 0, 16777215 PIXEL
        @ 002, 004 GET oMsg VAR cMsg OF oDlgMens MULTILINE SIZE 391, 121 FONT oFntTxt COLORS 0, 16777215 HSCROLL PIXEL
        @ 127, 134 BUTTON oBtnOk  PROMPT "&Ok" SIZE 051, 019 ACTION oDlgMens:End() OF oDlgMens PIXEL
    ACTIVATE MSDIALOG oDlgMens CENTERED

Return

/*/{Protheus.doc} M0605I
    Função que valida se apenas um título a pagar foi selecionado e chama a rotina de compensação.
    @type  Function
    @author Pono Tecnologia
    @since 29/08/2022
    @version 12.1.33
    @param cAlias , Character, Álias do arquivo temporário utilizado
    @param dDtIni , Date     , Data do início para ser considerado no filtro de títulos a compensar
    @param dDtFim , Date     , Data do fim para ser considerado no filtro de títulos a compensar
    @param nLimite, Numeric  , Valor limite para compensação
    @param nOpcao , Numeric  , Opção : 3 - Compensar ou 5 - Cancelar compensação
    /*/
User Function M0605I(cAlias,dDtIni,dDtFim,nLimite,nOpcao,cMsg)
    Local lRet      := .T.
    Local lPagar    := .F.
    Local lContinua := .T.
    Local cTitPcom  := ""
    Local lPagSel   := .F.
    Local lRecSel   := .F.
    
    (cAlias)->(dbGoTop())

    While (cAlias)->(!Eof())
        If !Empty((cAlias)->TMP_OK)
            If (cAlias)->TMP_TIPO == "P" .AND. !lPagar
                lPagar := .T.
                cTitPcom := (cAlias)->TMP_TITULO
            ElseIf (cAlias)->TMP_TIPO != "R"
                lContinua := .F.
                cMsg := "Apenas um registro a pagar pode ser selecionado por vez."
            EndIf
        EndIf
    (cAlias)->(DbSkip())
    Enddo

    (cAlias)->(dbGoTop())

    If nOpcao == 5
        While (cAlias)->(!Eof())
            If !Empty((cAlias)->TMP_OK)
                If (cAlias)->TMP_TIPO == "R" .AnD. (cAlias)->TMP_TITPG <> cTitPcom
                    lContinua := .F.
                    cMsg := "Apenas títulos a receber compensados com o título " + cTitPcom + " podem ser selecionados."
                EndIf
            EndIf
        (cAlias)->(DbSkip())
        Enddo
    EndIf

    (cAlias)->(dbGoTop())
    
    If lContinua
        While (cAlias)->(!Eof())
            If !Empty((cAlias)->TMP_OK)
                If (cAlias)->TMP_TIPO == "P"
                    lPagSel := .T.
                ElseIf (cAlias)->TMP_TIPO == "R"
                    lRecSel := .T.
                EndIf
            EndIf
        (cAlias)->(DbSkip())
        Enddo

        If !lPagSel
            lContinua := .F.
            cMsg := "Pelo menos um título a pagar deve ser selecionado."
        EndIf

        If !lRecSel
            lContinua := .F.
            cMsg := "Pelo menos um título a receber deve ser selecionado."
        EndIf

        If !lPagSel .AND. !lRecSel
            lContinua := .F.
            cMsg := "Nenhum título foi selecionado."
        EndIf

        (cAlias)->(dbGoTop())
    EndIf
    
    If lContinua    
        FwMsgRun(,{ || U_M0605C(cAlias,dDtIni,dDtFim,nLimite,nOpcao) }, "Compensação de títulos", 'Processando dados...')
    Else
        lRet := .F.
    EndIf

Return(lRet)

/*/{Protheus.doc} M0605J
    Função responsável pelo cancelamento da compensação de títulos da Koncili
    @type  Function
    @author Pono Tecnologia
    @since 29/08/2022
    @version 12.1.33
    @param cMarket, Character, Código do marketplace
    @param cMktId , Character, Id do marketplace
    /*/
User Function M0605J(cMarket,cMktId)
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
    Local TRB3     := ""
    Local nOpcao   := 5
    Local aFornece := {}
    Local cMsg     := ""
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
                        {"TMP_NOME"     ,"C",30,0},;
                        {"TMP_TITPG"    ,"C",09,0}}

    Local aCpoBro	:= {{"TMP_OK"	    ,, " "                  ,"  "},;
                        {"TMP_TIPO"     ,, "Carteira"           ,"!"},;
                        {"TMP_TITULO"	,, "Numero Titulo"      ,"@X"},;
                        {"TMP_PREFIX"	,, "Prefixo"            ,"@X"},;
                        {"TMP_PARCEL"	,, "Parcela"            ,"@X"},;
                        {"TMP_TPTIT"	,, "Tipo do Título"     ,"@X"},;
                        {"TMP_VLPAG"	,, "Valor Pagar"        ,"@E 999,999,999.99"},;
                        {"TMP_VLREC"	,, "Valor Receber"      ,"@E 999,999,999.99"},;
                        {"TMP_EMISSA"	,, "Data Emissao"       ,"@X"},;
                        {"TMP_VENCTO"	,, "Data Vencimento"    ,"@X"},;
                        {"TMP_SALDO"	,, "Saldo Titulo"       ,"@E 9,999,999,999.99"},;
                        {"TMP_JUROS"	,, "Juros"              ,"@E 9,999,999,999.99"},;
                        {"TMP_MULTA"	,, "Multa"              ,"@E 9,999,999,999.99"},;
                        {"TMP_DESCON"	,, "Descontos"          ,"@E 9,999,999,999.99"},;
                        {"TMP_ACRESC"	,, "Acrescimos"         ,"@E 9,999,999,999.99"},;
                        {"TMP_DESCRE"	,, "Decrescimos"        ,"@E 9,999,999,999.99"},;
                        {"TMP_CLIFOR"	,, "Cli/For"            ,"@X"},;
                        {"TMP_LOJA"	    ,, "Loja"               ,"@X"},;
                        {"TMP_NOME"		,, "Nome"               ,"@X"},;
                        {"TMP_TITPG"	,, "Titulo a Pagar"     ,"@X"}}

    Pergunte("M0605",.T.)

    dDtIni := MV_PAR01
    dDtFim := MV_PAR02
    nLimite:= MV_PAR03

    If Select("TRB3") > 0
        dbSelectArea("TRB3")
        dbCloseArea()
    EndIf

    aCarrega := {}

    aFornece := U_M0605N(cMktId)
    
    If Len(aFornece) > 0
        TRB3 := U_M0605K(aCampos,dDtIni,dDtFim,cMarket,aFornece,@nTotalP,@nTotalR)
    Else 
        Aviso("Atenção!","Amarração fornecedor X marketplace não encontrada.",{"Ok"})
        Return
    EndIf

    DbSelectArea("TRB3")
    TRB3->(DbGoTop())

    DEFINE FONT oFnt NAME "Arial" SIZE 12,14 BOLD

    aSize := MSADVSIZE()

    DEFINE MSDIALOG oDlg TITLE "Cancelamento de Compensação Entre Carteiras" From aSize[7],0 To aSize[6],aSize[5] OF oMainWnd PIXEL
    oDlg:lMaximized := .T.

    oMark := MsSelect():New("TRB3","TMP_OK","",aCpoBro,@lInverte,@cMarca,{50,oDlg:nLeft,oDlg:nBottom,oDlg:nRight})
    oMark:bMark := {| | U_M0605D("TRB3",cMarca,lInverte,@nSelecP,@NSelecR,oSelecP,oSelecR)}
    oMark:oBrowse:lhasMark = .T.
    oMark:oBrowse:lCanAllmark := .T.
                                
    oMark:oBrowse:bAllMark := { || U_M0605G("TRB3",cMarca,lInverte,@nSelecP,@NSelecR,oSelecP,oSelecR),oMark:oBrowse:Refresh(.T.)}
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

    ACTIVATE MSDIALOG oDlg ON INIT EnchoiceBar(oDlg,{|| IIF(U_M0605I("TRB3",dDtIni,dDtFim,nLimite,nOpcao,@cMsg),oDlg:End(),Aviso("Atenção!",cMsg,{"Ok"}))},{|| oDlg:End()},,aBut450)

Return

/*/{Protheus.doc} M0605K
    Função responsável pela montagem do select para cancelamento da compensação de títulos.
    @type  Function
    @author Pono Tecnologia
    @since 29/08/2022
    @version 12.1.33+
    /*/
User Function M0605K(aCampos,dDtIni,dDtFim,cMarket,aFornece,nTotalP,nTotalR)
    Local nX

    aCarrega := {}

    If(Type('oTable') <> 'U')
        oTable:Delete()
        oTable := Nil
    EndIf

    oTable := FwTemporaryTable():New('TRB3')
    oTable:SetFields(aCampos)

    oTable:Create()

    U_M0605L(aCarrega,dDtIni,dDtFim,cMarket,nTotalP,nTotalR,aFornece) // Carrega títulos compensados para o fornecedor informado

    If Len(aCarrega) > 0

        For nX := 1 To Len(aCarrega)

            DbSelectArea('TRB3')

            RecLock('TRB3', .T.)
        
                TRB3->TMP_TIPO      := aCarrega[nX,1]
                TRB3->TMP_TITULO    := aCarrega[nX,2]
                TRB3->TMP_PREFIX    := aCarrega[nX,16]
                TRB3->TMP_PARCEL    := aCarrega[nX,17]
                TRB3->TMP_TPTIT     := aCarrega[nX,18]
                TRB3->TMP_VLPAG     := aCarrega[nX,3]
                TRB3->TMP_VLREC     := aCarrega[nX,4]
                TRB3->TMP_EMISSA    := STOD(aCarrega[nX,5])
                TRB3->TMP_VENCTO    := STOD(aCarrega[nX,6])
                TRB3->TMP_SALDO     := aCarrega[nX,7]
                TRB3->TMP_JUROS     := aCarrega[nX,8]
                TRB3->TMP_MULTA     := aCarrega[nX,9]
                TRB3->TMP_DESCON    := aCarrega[nX,10]
                TRB3->TMP_ACRESC    := aCarrega[nX,11]
                TRB3->TMP_DESCRE    := aCarrega[nX,12]
                TRB3->TMP_CLIFOR    := aCarrega[nX,13]
                TRB3->TMP_LOJA      := aCarrega[nX,14]
                TRB3->TMP_NOME      := aCarrega[nX,15]
                TRB3->TMP_TITPG     := aCarrega[nX,19]

            TRB3->(MsUnlock())
        
        Next nX

        TRB3->(DbGoTop())

    EndIf

Return

/*/{Protheus.doc} M0605L
    Função que busca os títulos compensados que serão listados para cancelamento da compensação.
    @type  Function
    @author Pono Tecnologia
    @since 29/08/2022
    /*/
User Function M0605L(aCarrega,dDtIni,dDtFim,cMarket,nTotalP,nTotalR,aFornece)
    Local cAlias   := GetNextAlias()
    Local cQuery
    Local nX
    Local cTitPag

    cQuery := "SELECT * "
    cQuery += "FROM " + RetSqlName("SE1") + " "
    cQuery += "WHERE D_E_L_E_T_ = ' ' "
    cQuery += "AND E1_FILIAL = '" + xFilial("SE1") + "' "
    cQuery += "AND E1_NUMLIQ != '' "
    cQuery += "AND E1_STATUS = 'B' "
    cQuery += "AND E1_EMISSAO >= '" + DTOS(dDtIni) + "' AND E1_EMISSAO <= '" + DTOS(dDtFim) + "' "
    cQuery += "AND E1_CODMKT = '" + cMarket + "' "
    cQuery += "AND E1_RESOLKO = 'S'"

    MPSysOpenQuery( cQuery, cAlias )

    DBSelectArea(cAlias)
    (cAlias)->(dbGoTop())
    
    While !(cAlias)->((Eof()))
        If !Empty(Alltrim((cAlias)->E1_NUM))
            cTitPag := U_M0605O((cAlias)->E1_IDENTEE,(cAlias)->E1_CLIENTE,(cAlias)->E1_LOJA)
            AADD( aCarrega, { "R", (cAlias)->E1_NUM, 0, (cAlias)->E1_VALOR, (cAlias)->E1_EMISSAO, (cAlias)->E1_VENCTO, (cAlias)->E1_SALDO, (cAlias)->E1_JUROS, (cAlias)->E1_MULTA, (cAlias)->E1_DESCONT, (cAlias)->E1_ACRESC, (cAlias)->E1_DECRESC, (cAlias)->E1_CLIENTE, (cAlias)->E1_LOJA, (cAlias)->E1_NOMCLI, (cAlias)->E1_PREFIXO, (cAlias)->E1_PARCELA, (cAlias)->E1_TIPO, cTitPag } )
            For nX := 1 To Len(aFornece)
                U_M0605M((cAlias)->E1_IDENTEE,aFornece[nX,1],aFornece[nX,2],@aCarrega,nTotalP)
            Next nX
            nTotalR += (cAlias)->E1_SALDO
        EndIf
        (cAlias)->(dbSkip())
    EndDo
    
    aSort(aCarrega,,,{|x,y|x[1] < y[1]})
    
    (cAlias)->(dbCloseArea())    

Return

/*/{Protheus.doc} M0605M
    Função responsável se os títulos selecionados já foram compensados.
    @type  Function
    @author Pono Tecnologia
    @since 29/08/2022
    @version 12.1.33
    /*/
User Function M0605M(cComp,cFornece,cLojFor,aCarrega,nTotalP)
    Local cAlias   := GetNextAlias()
    Local cQuery
    Local nPos     := 0

    cQuery := "SELECT * "
    cQuery += "FROM " + RetSqlName("SE5") + " E5 "
    cQuery += "INNER JOIN " + RetSqlName("SE2") + " E2 ON "
    cQuery += "E5.E5_FILIAL = E2.E2_FILIAL "
    cQuery += "AND E5.E5_FILIAL = E2.E2_FILIAL "
    cQuery += "AND E5.E5_PREFIXO = E2.E2_PREFIXO "
    cQuery += "AND E5.E5_NUMERO = E2.E2_NUM "
    cQuery += "AND E5.E5_PARCELA = E2.E2_PARCELA "
    cQuery += "AND E5.E5_TIPO = E2.E2_TIPO "
    cQuery += "AND E5.E5_CLIFOR = E2.E2_FORNECE "
    cQuery += "AND E5.E5_LOJA = E2.E2_LOJA "
    cQuery += "AND E2.D_E_L_E_T_ = ' ' "
    cQuery += "WHERE E5.D_E_L_E_T_ = ' ' "
    cQuery += "AND E5_FILIAL = '" + xFilial("SE5") + "' "
    cQuery += "AND E5_IDENTEE = '" + cComp + "' "
    cQuery += "AND E5_CLIFOR = '" + cFornece + "' "
    cQuery += "AND E5_LOJA = '" + cLojFor + "'"

    MPSysOpenQuery( cQuery, cAlias )

    DBSelectArea(cAlias)
    (cAlias)->(dbGoTop())
    
    If Len(cAlias) > 0
        nPos := 0
        nPos := aScan( aCarrega, {|x| AllTrim(x[2]) == Alltrim((cAlias)->E2_NUM) } )
        If nPos == 0 .AND. !Empty(Alltrim((cAlias)->E2_NUM))
            AADD( aCarrega, { "P", (cAlias)->E2_NUM, (cAlias)->E2_SALDO, 0, (cAlias)->E2_EMISSAO, (cAlias)->E2_VENCTO, (cAlias)->E2_SALDO, (cAlias)->E2_JUROS, (cAlias)->E2_MULTA, (cAlias)->E2_DESCONT, (cAlias)->E2_ACRESC, (cAlias)->E2_DECRESC, (cAlias)->E2_FORNECE, (cAlias)->E2_LOJA, (cAlias)->E2_NOMFOR, (cAlias)->E2_PREFIXO, (cAlias)->E2_PARCELA, (cAlias)->E2_TIPO, "" } )
            nTotalP += (cAlias)->E2_SALDO
        EndIf
    EndIf
    
    (cAlias)->(dbCloseArea())    

Return

/*/{Protheus.doc} M0605N
    Função responsável por retornar os fornecedores cadastrados para o marketplace selecioando.
    @type  Function
    @author Pono Tecnologia
    @since 27/09/2022
    @version 12.1.33+
    @param cMktId, Character, Código do marketplace
    @return Array, Array contendo os códigos e lojas dos fornecedores cadastrados para o marketplace selecionado.
    /*/
User Function M0605N(cMktId)
    Local cAlias   := GetNextAlias()
    Local cQuery
    Local aFornece := {}

    cQuery := "SELECT * "
    cQuery += "FROM " + RetSqlName("ZZE") + " "
    cQuery += "WHERE D_E_L_E_T_ = ' ' "
    cQuery += "AND ZZE_FILIAL = '" + xFilial("ZZE") + "' "
    cQuery += "AND ZZE_MKTID = '" + cMktId + "' "
    cQuery += "AND ZZE_ATIVO = 'S' "

    MPSysOpenQuery( cQuery, cAlias )

    DBSelectArea(cAlias)
    (cAlias)->(dbGoTop())
    
    While !(cAlias)->((Eof()))
        AADD( aFornece, { (cAlias)->ZZE_CODFOR, (cAlias)->ZZE_LOJFOR } )
        (cAlias)->(dbSkip())
    EndDo
    
    (cAlias)->(dbCloseArea())

Return(aFornece)

/*/{Protheus.doc} M0605O
    Função responsável por retornar o número do título utilizado na compensação.
    @type  Function
    @author Pono Tecnologia
    @since 29/09/2022
    @version 12.1.33+
    /*/
User Function M0605O(cComp,cCliente,cLojCli)
    Local cAlias   := GetNextAlias()
    Local cQuery
    Local cTitPag

    cQuery := "SELECT * "
    cQuery += "FROM " + RetSqlName("SE5") + " E5 "
    cQuery += "WHERE E5.D_E_L_E_T_ = ' ' "
    cQuery += "AND E5_FILIAL = '" + xFilial("SE5") + "' "
    cQuery += "AND E5_IDENTEE = '" + cComp + "' "

    MPSysOpenQuery( cQuery, cAlias )

    DBSelectArea(cAlias)
    (cAlias)->(dbGoTop())
    
    While !(cAlias)->(Eof())
        If (cAlias)->E5_CLIFOR <> cCliente .AND. (cAlias)->E5_LOJA <> cLojCli
            cTitPag := (cAlias)->E5_NUMERO
        EndIf
    (cAlias)->(dbSkip())
    EndDo
    
    (cAlias)->(dbCloseArea())    

Return(cTitPag)

/*/{Protheus.doc} M0605P
    Função responsável pela validação da marcação e pela atualização das variáveis de totais da tela na rotina de cancelamento de compensação.
    @type  Function
    @author Pono Tecnologia
    @since 29/09/2022
    @version 12.1.33+
    /*/
User Function M0605P(cAlias,cMarca,lInverte,nSelecP,nSelecR,oSelecP,oSelecR)

    RecLock(cAlias,.F.)
        If Marked("TMP_OK")
            (cAlias)->TMP_OK := cMarca
            If (cAlias)->TMP_VLREC != 0
                nSelecR += (cAlias)->TMP_VLREC
            Else
                nSelecP += (cAlias)->TMP_VLPAG
            EndIf
        Else
            (cAlias)->TMP_OK := " "
            If (cAlias)->TMP_VLREC != 0
                nSelecR -= (cAlias)->TMP_VLREC
            Else
                nSelecP -= (cAlias)->TMP_VLPAG
            EndIf
        End
    MsUnlock()

    oSelecP:Refresh()
    oSelecR:Refresh()

Return
