unit OllamaAPIasync;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fphttpclient, fpjson, jsonparser, Forms, Controls, unit1;


type

  { TOllamaRequestThread }

  TOllamaRequestThread = class(TThread)
  private
    FEndpoint, FModel, FPrompt, FResponseContent, FError: string;
    FMessages: TStringList;
    FIsChat: Boolean;
    FStartTime, FEndTime: QWord;
    procedure UpdateUI;
    procedure HandleError;
    procedure ProcessStreamingResponse(ResponseStream: TStream);
    procedure TOllamaRequestThreadClearMemo;
    procedure ResponseTime;

  protected
    procedure Execute; override;
  public
    constructor Create(const Endpoint, Model, Prompt: string; IsChat: Boolean; Messages: TStringList);
    destructor Destroy; override;
  end;

procedure GenerateResponse;
procedure ChatWithModel;

implementation

constructor TOllamaRequestThread.Create(const Endpoint, Model, Prompt: string; IsChat: Boolean; Messages: TStringList);
begin
  inherited Create(True);
  FreeOnTerminate := True;
  FEndpoint := Endpoint;
  FModel := Model;
  FPrompt := Prompt;
  FIsChat := IsChat;
  FStartTime := GetTickCount64; //Time
  if Assigned(Messages) then
  begin
    FMessages := TStringList.Create;
    FMessages.Assign(Messages);
  end
  else
    FMessages := nil;
end;

destructor TOllamaRequestThread.Destroy;
begin
  if Assigned(FMessages) then
    FMessages.Free;
  inherited Destroy;
end;

procedure TOllamaRequestThread.UpdateUI;
begin
  Form1.MemoAnswer.Lines.Text := Form1.MemoAnswer.Lines.Text + FResponseContent;
end;

procedure TOllamaRequestThread.HandleError;
begin
  Form1.MemoAnswer.Lines.Add('Error: ' + FError);
end;

function ReadStreamLine(Stream: TStream): String;
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

procedure TOllamaRequestThread.ProcessStreamingResponse(ResponseStream: TStream);
var
  Line: String;
  JsonData: TJSONData;
  Content: String;
begin
  ResponseStream.Position := 0;
  Synchronize(@TOllamaRequestThreadClearMemo);

  while (ResponseStream.Position < ResponseStream.Size) and not Terminated do
  begin
    Line := ReadStreamLine(ResponseStream);
    if Line = '' then Continue;
    Line := Trim(Line);

    try
      JsonData := GetJSON(Line);
      try
        if FIsChat then
          Content := JsonData.GetPath('message.content').AsString
        else
          Content := JsonData.GetPath('response').AsString;

        FResponseContent := Content;
        Synchronize(@UpdateUI);

        if JsonData.FindPath('done') <> nil then
          if JsonData.GetPath('done').AsBoolean then Break;
      finally
        JsonData.Free;
      end;

    except
      on E: Exception do
      begin
        FError := E.Message;
        Synchronize(@HandleError);
      end;
    end;
  end;
  FEndTime := GetTickCount64;
  Synchronize(@ResponseTime);

end;

procedure TOllamaRequestThread.TOllamaRequestThreadClearMemo;
begin
  Form1.MemoAnswer.Clear;
end;

procedure TOllamaRequestThread.ResponseTime;
begin
   //Form1.MemoAnswer.Lines.Add('Response time: ' + IntToStr(FEndTime - FStartTime) + ' ms');
   Form1.MemoAnswer.Lines.Add('Response time: ' + FormatFloat('0.00', (FEndTime - FStartTime) / 1000) + ' s');
end;

procedure TOllamaRequestThread.Execute;
var
  Client: TFPHTTPClient;
  RequestData: TJSONObject;
  ResponseStream: TMemoryStream;
  MessagesArray: TJSONArray;
  i: Integer;
  TimeoutValue: Integer;
begin
  Client := TFPHTTPClient.Create(nil);
  RequestData := TJSONObject.Create;
  ResponseStream := TMemoryStream.Create;
  TimeoutValue := StrToIntDef(GetEnvironmentVariable('OLLAMA_API_TIMEOUT'), 5000);

  try
    Client.IOTimeout := TimeoutValue;
    RequestData.Add('model', FModel);
    RequestData.Add('stream', True);

    if not FIsChat then
      RequestData.Add('prompt', FPrompt)
    else
    begin
      MessagesArray := TJSONArray.Create;
      for i := 0 to FMessages.Count - 1 do
        MessagesArray.Add(TJSONObject.Create(['role', 'user', 'content', FMessages[i]]));
      RequestData.Add('messages', MessagesArray);
    end;

    Client.RequestBody := TStringStream.Create(RequestData.AsJSON, TEncoding.UTF8);
    Client.AddHeader('Content-Type', 'application/json');

    try
      Client.HTTPMethod('POST', 'http://localhost:11434/api/' + FEndpoint, ResponseStream, [200]);
      ProcessStreamingResponse(ResponseStream);
    except
      on E: Exception do
      begin
        FError := 'Connection error: ' + E.Message;
        Synchronize(@HandleError);
      end;
    end;
  finally
    RequestData.Free;
    Client.RequestBody.Free;
    Client.Free;
    ResponseStream.Free;
  end;
end;

procedure SendOllamaRequest(const Endpoint: String; IsChat: Boolean);
var
  Thread: TOllamaRequestThread;
  Messages: TStringList;
begin
  if IsChat then
  begin
    Messages := TStringList.Create;
    Messages.Add(Form1.MemoAsk.Lines.Text);
  end
  else
    Messages := nil;

  Thread := TOllamaRequestThread.Create(
    Endpoint,
    Form1.ComboModel.Text,
    Form1.ComboPromt.Text + Form1.MemoAsk.Lines.Text,
    IsChat,
    Messages
  );
  Thread.Start;
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

