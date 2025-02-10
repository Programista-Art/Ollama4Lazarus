unit OllamaAPI;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fphttpclient, fpjson, jsonparser, Forms, Controls,unit1;

const
  //MODEL_NAME = 'deepseek-r1:1.5b';
  API_TIMEOUT = 30000;


procedure GenerateResponse;
procedure ChatWithModel;

implementation

function ReadStreamLine(Stream: TStream): String;
const
  BUFFER_SIZE = 1024;
var
  C: Char;
  BytesRead: Integer;
begin
  Result := '';
  repeat
    BytesRead := Stream.Read(C, SizeOf(Char));
    if BytesRead = 0 then Break;
    if C = #10 then Break;
    if C <> #13 then Result += C;
  until False;
end;

procedure ProcessStreamingResponse(ResponseStream: TStream; IsChat: Boolean);
var
  Line: String;
  JsonData: TJSONData;
  Content: String;
begin
  ResponseStream.Position := 0;
  Form1.MemoAnswer.Clear;

  while ResponseStream.Position < ResponseStream.Size do
  begin
    Line := ReadStreamLine(ResponseStream);
    Line := Trim(Line);

    if Line = '' then Continue;

    try
      JsonData := GetJSON(Line);
      try
        if IsChat then
          Content := JsonData.GetPath('message.content').AsString
        else
          Content := JsonData.GetPath('response').AsString;

        Form1.MemoAnswer.Lines.Text := Form1.MemoAnswer.Lines.Text + Content;


        if JsonData.GetPath('done').AsBoolean then Break;
      finally
        JsonData.Free;
      end;
    except
      on E: Exception do
        Form1.MemoAnswer.Lines.Add('Error: ' + E.Message);
    end;
    Application.ProcessMessages;
  end;
end;

procedure SendOllamaRequest(const Endpoint: String; IsChat: Boolean);
var
  Client: TFPHTTPClient;
  RequestData: TJSONObject;
  ResponseStream: TMemoryStream;
  MessagesArray: TJSONArray;
begin
  Client := TFPHTTPClient.Create(nil);
  RequestData := TJSONObject.Create;
  ResponseStream := TMemoryStream.Create;

  try
    Client.IOTimeout := API_TIMEOUT;
    RequestData.Add('model', Form1.ComboModel.Text);
    //RequestData.Add('model', MODEL_NAME);
    RequestData.Add('stream', True);
    //RequestData.Add('format', 'json');

    if not IsChat then
    begin
      if Pos('deepseek', LowerCase(Form1.ComboModel.Text)) > 0 then
        RequestData.Add('prompt', '### Instruction:' + #13#10 +
          Form1.MemoAsk.Lines.Text + #13#10#13#10 + '### Response:')
      else
        //RequestData.Add('prompt', Form1.MemoAsk.Lines.Text);
        RequestData.Add('prompt', Form1.ComboPromt.Text + Form1.MemoAsk.Lines.Text);

    end
    else
    begin
      MessagesArray := TJSONArray.Create;
      try
        MessagesArray.Add(TJSONObject.Create(
          ['role', 'user', 'content', Form1.MemoAsk.Lines.Text]
        ));
        RequestData.Add('messages', MessagesArray);
      except
        MessagesArray.Free;
        raise;
      end;
    end;

    Form1.MemoAnswer.Lines.Add('Inquiry: ' + RequestData.AsJSON);

    Client.RequestBody := TStringStream.Create(RequestData.AsJSON, TEncoding.UTF8);
    Client.AddHeader('Content-Type', 'application/json');

    try
      Client.HTTPMethod('POST', 'http://localhost:11434/api/' + Endpoint, ResponseStream, [200]);
      ResponseStream.Position := 0;
      ProcessStreamingResponse(ResponseStream, IsChat);
    except
      on E: Exception do
        Form1.MemoAnswer.Lines.Add('Connection error: ' + E.Message);
    end;

  finally
    RequestData.Free;
    Client.RequestBody.Free;
    Client.Free;
    ResponseStream.Free;
  end;
end;

procedure GenerateResponse;
begin
  SendOllamaRequest('generate', False);
end;

procedure ChatWithModel;
begin
  SendOllamaRequest('chat', True);
end;

end.
