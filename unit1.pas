unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls,
  fphttpclient, opensslsockets, fpjson, jsonparser;

type

  { TForm1 }

  TForm1 = class(TForm)
    btnConsultar: TButton;
    btnBaixarXML: TButton;
    edtAPIKey: TEdit;
    edtChaveAcesso: TEdit;
    lblAPIKey: TLabel;
    lblChaveAcesso: TLabel;
    SaveDialog1: TSaveDialog;
    procedure btnConsultarClick(Sender: TObject);
    procedure btnBaixarXMLClick(Sender: TObject); // <-- CORREÇÃO 1: Procedimento declarado
  private

  public

  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.btnConsultarClick(Sender: TObject);
var
  HttpClient: TFPHttpClient;
  URL: string;
  ChaveAcesso: string;
  APIKey: string;
  ResponseString: string;
  JSONData: TJSONData;
  JSONObject: TJSONObject;
  Status: string;
  StatusMsg: string;
begin
  // 1. Obter os valores dos Edits (usando Trim para remover espaços)
  ChaveAcesso := Trim(edtChaveAcesso.Text);
  APIKey := Trim(edtAPIKey.Text);

  // Validação simples
  if (ChaveAcesso = '') or (APIKey = '') then
  begin
    ShowMessage('Por favor, preencha a API Key e a Chave de Acesso.');
    Exit;
  end;

  // Validação para evitar o Erro 400 (Bad Request)
  if Length(ChaveAcesso) <> 44 then
  begin
    ShowMessage('Erro: A Chave de Acesso deve conter exatamente 44 números.');
    Exit;
  end;

  // 2. Montar a URL
  URL := 'https://api.meudanfe.com.br/v2/fd/add/' + ChaveAcesso;

  HttpClient := TFPHttpClient.Create(nil);
  try
    // 3. Adicionar a API Key ao Header
    HttpClient.RequestHeaders.Add('Api-Key: ' + APIKey);
    HttpClient.RequestHeaders.Add('Content-Length: 0');

    // 4. Executar a requisição PUT
    try
      ResponseString := HttpClient.Put(URL);

      // 5. Verificar se a requisição foi bem-sucedida (HTTP 200)
      if HttpClient.ResponseStatusCode = 200 then
      begin
        // 6. Processar o JSON
        try
          JSONData := GetJSON(ResponseString);
          if JSONData <> nil then
          try
            JSONObject := JSONData as TJSONObject;
            Status := JSONObject.Find('status').AsString;
            StatusMsg := JSONObject.Find('statusMessage').AsString;
            ShowMessage('Status da API: ' + Status + ' - ' + StatusMsg);
          finally
            JSONData.Free;
          end
          else
          begin
            ShowMessage('Erro: Não foi possível ler o JSON da resposta.');
          end;
        except
          on E: Exception do
            ShowMessage('Erro ao processar o JSON: ' + E.Message);
        end;
      end
      else
      begin
        ShowMessage('Erro na requisição HTTP: ' + IntToStr(HttpClient.ResponseStatusCode) + ' - ' + HttpClient.ResponseStatusText);
      end;

    except
      on E: Exception do
      begin
        ShowMessage('Erro ao tentar conectar: ' + E.Message);
      end;
    end;

  finally
    HttpClient.Free;
  end;
end;

// <-- CORREÇÃO 2: Nome do procedimento corrigido para TForm1
procedure TForm1.btnBaixarXMLClick(Sender: TObject);
var
  HTTPClient: TFPHTTPClient;
  URL, ChaveAcesso, SuaApiKey, RespostaJSON, XMLString, NomeCampoXML: string;
  JSONData: TJSONData;
  JSONObject: TJSONObject;
  SL: TStringList;
  JSONStringValue: TJSONString; // <-- CORREÇÃO: Variável para o tipo JSON
  ErrorMessage: string;
begin
  // --- 1. Configuração Inicial ---
  ChaveAcesso := Trim(edtChaveAcesso.Text);
  SuaApiKey   := Trim(edtAPIKey.Text);

  // !!! ATENÇÃO AQUI !!!
  // Mude 'xml' para o nome EXATO do campo que a API retorna.
  NomeCampoXML := 'data';

  if ChaveAcesso = '' then
  begin
    ShowMessage('Por favor, informe a Chave de Acesso.');
    edtChaveAcesso.SetFocus;
    Exit;
  end;

  if SuaApiKey = '' then
  begin
    ShowMessage('Erro: Por favor, preencha a Api-Key.');
    edtAPIKey.SetFocus;
    Exit;
  end;

  URL := 'https://api.meudanfe.com.br/v2/fd/get/xml/' + ChaveAcesso;

  // --- 2. Preparar e Executar a Requisição HTTP ---
  HTTPClient := TFPHTTPClient.Create(nil);
  try
    HTTPClient.RequestHeaders.Add('Api-Key: ' + SuaApiKey);
    HTTPClient.RequestHeaders.Add('Accept: application/json');

    try
      RespostaJSON := HTTPClient.Get(URL);
    except
      On E: Exception do
      begin
        ShowMessage('Falha ao conectar à API: ' + E.Message);
        Exit;
      end;
    end;

    // --- 3. Analisar (Parse) o JSON de Resposta ---
    try
      JSONData := GetJSON(RespostaJSON);
      if JSONData = nil then
      begin
          ShowMessage('A resposta da API não é um JSON válido (GetJSON retornou nil).');
          Exit;
      end;

      if not (JSONData is TJSONObject) then
      begin
        ShowMessage('A resposta da API não é um objeto JSON válido.');
        JSONData.Free;
        Exit;
      end;

      JSONObject := JSONData as TJSONObject;

      // CORREÇÃO (Erro "TryGetValue"): Usando Find + TJSONString
      // Tenta encontrar o campo que contém o XML
      if JSONObject.Find(NomeCampoXML, JSONStringValue) then
      begin
        // Sucesso! Extrai a string do objeto JSON
        XMLString := JSONStringValue.AsString;
      end
      else
      begin
        // Se não encontrar o XML, tenta ler uma mensagem de erro da API
        ErrorMessage := ''; // Limpa a variável de erro
        if JSONObject.Find('message', JSONStringValue) then
        begin
            ErrorMessage := JSONStringValue.AsString;
        end;

        if ErrorMessage <> '' then
          ShowMessage('Erro retornado pela API: ' + ErrorMessage)
        else
          ShowMessage('Erro: Não foi possível encontrar o campo "' + NomeCampoXML + '" no JSON de resposta. Verifique a documentação.');

        JSONData.Free;
        Exit;
      end;

      // --- 4. Salvar o XML extraído em um Arquivo ---
      SaveDialog1.FileName := ChaveAcesso + '.xml';
      SaveDialog1.Filter := 'Arquivos XML (*.xml)|*.xml';

      if SaveDialog1.Execute then
      begin
        SL := TStringList.Create;
        try
          SL.Text := XMLString;
          SL.SaveToFile(SaveDialog1.FileName);
          ShowMessage('XML baixado e salvo com sucesso em: ' + SaveDialog1.FileName);
        finally
          SL.Free;
        end;
      end;

    finally
      if Assigned(JSONData) then
        JSONData.Free;
    end;

  finally
    HTTPClient.Free;
  end;
end;

end.
