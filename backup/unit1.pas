unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls,
  fphttpclient, opensslsockets, fpjson, jsonparser;

type

  { TForm1 }

  TForm1 = class(TForm)
    btnConsultar: TButton; // Botão principal (pode renomear para "Baixar XML")
    btnBaixarXML: TButton; // Você pode apagar este
    edtChaveAcesso: TEdit; // Campo para a chave da NFe
    lblChaveAcesso: TLabel; // Label para a chave da NFe
    SaveDialog1: TSaveDialog;
    procedure btnConsultarClick(Sender: TObject);
    procedure btnBaixarXMLClick(Sender: TObject);
  private
    function ExecutarConsulta: Boolean;
    function ExecutarDownload: Boolean;
  public

  end;

var
  Form1: TForm1;

implementation

// --- MUDANÇA 1: Defina sua API Key aqui ---
const
  MINHA_API_KEY = '713c2342-69fa-43db-bb96-f69713df2407';

{$R *.lfm}

{ TForm1 }

function TForm1.ExecutarConsulta: Boolean;
var
  HttpClient: TFPHttpClient;
  URL: string;
  ChaveAcesso: string;
  APIKey: string; // A variável continua existindo...
  ResponseString: string;
  JSONData: TJSONData;
  JSONObject: TJSONObject;
begin
  Result := False;

  ChaveAcesso := Trim(edtChaveAcesso.Text);
  // --- MUDANÇA 2: Usamos a constante ---
  APIKey := MINHA_API_KEY;

  // --- MUDANÇA 3: Simplificamos a validação ---
  if (ChaveAcesso = '') then
  begin
    ShowMessage('Por favor, preencha a Chave de Acesso.');
    Exit;
  end;

  if Length(ChaveAcesso) <> 44 then
  begin
    ShowMessage('Erro: A Chave de Acesso deve conter exatamente 44 números.');
    Exit;
  end;

  URL := 'https://api.meudanfe.com.br/v2/fd/add/' + ChaveAcesso;

  HttpClient := TFPHttpClient.Create(nil);
  try
    HttpClient.RequestHeaders.Add('Api-Key: ' + APIKey);
    HttpClient.RequestHeaders.Add('Content-Length: 0');

    try
      ResponseString := HttpClient.Put(URL);

      if HttpClient.ResponseStatusCode = 200 then
      begin
        try
          JSONData := GetJSON(ResponseString);
          if JSONData <> nil then
          try
            JSONObject := JSONData as TJSONObject;
            Status := JSONObject.Find('status').AsString;
            StatusMsg := JSONObject.Find('statusMessage').AsString;

            ShowMessage('Clique em OK para salvar o XML ' );
            Result := True;

          finally
            JSONData.Free;
          end
          else
          begin
            ShowMessage('Erro: Não foi possível ler o JSON da resposta da consulta.');
          end;
        except
          on E: Exception do
            ShowMessage('Erro ao processar o JSON da consulta: ' + E.Message);
        end;
      end
      else
      begin
        ShowMessage('Erro na requisição (Consulta): ' + IntToStr(HttpClient.ResponseStatusCode) + ' - ' + HttpClient.ResponseStatusText);
      end;
    except
      on E: Exception do
      begin
        ShowMessage('Erro ao tentar conectar (Consulta): ' + E.Message);
      end;
    end;
  finally
    HttpClient.Free;
  end;
end;

function TForm1.ExecutarDownload: Boolean;
var
  HTTPClient: TFPHTTPClient;
  URL, ChaveAcesso, SuaApiKey, RespostaJSON, XMLString, NomeCampoXML: string;
  JSONData: TJSONData;
  JSONObject: TJSONObject;
  SL: TStringList;
  JSONStringValue: TJSONString;
  ErrorMessage: string;
begin
  Result := False;

  ChaveAcesso := Trim(edtChaveAcesso.Text);
  // --- MUDANÇA 4: Usamos a constante aqui também ---
  SuaApiKey   := MINHA_API_KEY;
  NomeCampoXML := 'data';

  URL := 'https://api.meudanfe.com.br/v2/fd/get/xml/' + ChaveAcesso;

  HTTPClient := TFPHTTPClient.Create(nil);
  try
    HTTPClient.RequestHeaders.Add('Api-Key: ' + SuaApiKey);
    HTTPClient.RequestHeaders.Add('Accept: application/json');

    try
      RespostaJSON := HTTPClient.Get(URL);
    except
      On E: Exception do
      begin
        ShowMessage('Falha ao conectar à API (Download): ' + E.Message);
        Exit;
      end;
    end;

    try
      JSONData := GetJSON(RespostaJSON);
      if JSONData = nil then
      begin
          ShowMessage('A resposta da API (Download) não é um JSON válido.');
          Exit;
      end;

      if not (JSONData is TJSONObject) then
      begin
        ShowMessage('A resposta da API (Download) não é um objeto JSON válido.');
        JSONData.Free;
        Exit;
      end;

      JSONObject := JSONData as TJSONObject;

      if JSONObject.Find(NomeCampoXML, JSONStringValue) then
      begin
        XMLString := JSONStringValue.AsString;
      end
      else
      begin
        ErrorMessage := '';
        if JSONObject.Find('message', JSONStringValue) then
        begin
            ErrorMessage := JSONStringValue.AsString;
        end;

        if ErrorMessage <> '' then
          ShowMessage('Erro retornado pela API (Download): ' + ErrorMessage)
        else
          ShowMessage('Erro: Não foi possível encontrar o campo "' + NomeCampoXML + '" no JSON de resposta.');

        JSONData.Free;
        Exit;
      end;

      SaveDialog1.FileName := ChaveAcesso + '.xml';
      SaveDialog1.Filter := 'Arquivos XML (*.xml)|*.xml';

      if SaveDialog1.Execute then
      begin
        SL := TStringList.Create;
        try
          SL.Text := XMLString;
          SL.SaveToFile(SaveDialog1.FileName);
          ShowMessage('XML baixado e salvo com sucesso em: ' + SaveDialog1.FileName);
          Result := True;
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

procedure TForm1.btnConsultarClick(Sender: TObject);
begin
  if ExecutarConsulta then
  begin
    ExecutarDownload;
  end;
end;

procedure TForm1.btnBaixarXMLClick(Sender: TObject);
begin
  // Não é mais necessário, mas pode deixar
end;

end.
