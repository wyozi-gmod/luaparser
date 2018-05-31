local function OOP(name, parent, ...)
	local t = {}

	local meta = {}
	meta.__call = function(_, ...)
		local inst = setmetatable({}, {__index = t})
		if inst.initialize then inst:initialize(...) end
		return inst
	end
	if parent then meta.__index = parent end

	setmetatable(t, meta)

	return t
end

local Lexer = {}

Lexer.KEYWORDS = {'break', 'do', 'end', 'else',
	'elseif', 'function', 'if', 'local',
	'nil', 'not', 'or', 'repeat',
	'return', 'then', 'until', 'while'}


function Lexer.isWhitespace(c)
	return c == " " or c == "\t" or c == "\n" or c == "\r"
end
function Lexer.isNumber(c)
	return c:match("%d") ~= nil
end
function Lexer.isIdentifier(c)
	return c:match("[%d_%a]") ~= nil
end

function Lexer.tokenize(str)
	local tokens = {}

	local i = 0
	local line, col = 1, 1

	local function cur()
		if i > #str then return nil end
		return string.sub(str, i, i)
	end
	local function next()
		i = i+1

		local c = cur()

		if c == "\n" then
			line = line + 1
			col = 1
		else
			col = col + 1
		end

		return c
	end
	local function peek()
		local pi = i+1
		if pi > #str then return nil end
		return string.sub(str, pi, pi)
	end

	-- Go next until match is not found
	local function nextwhilef(fn)
		local t = {}
		while true do
			local n = peek()

			if not n or not fn(n) then
				return table.concat(t, "")
			end

			t[#t+1] = n

			next()
		end
	end
	local function nextwhilem(patt)
		return nextwhilef(function(n) return string.match(n, patt) end)
	end
	local function nextwhilenm(patt)
		return nextwhilef(function(n) return string.match(n, patt) == nil end)
	end

	local function add(token, str)
		tokens[#tokens+1] = {token = token, str = str, col = col-#str, line = line}
	end

	while true do
		local c = next()
		if not c then break end

		if Lexer.isWhitespace(c) then
		elseif Lexer.isNumber(c) then
			local str = c .. nextwhilem("[%d%.]")
			add("number", str)
		elseif Lexer.isIdentifier(c) then
			local str = c .. nextwhilef(Lexer.isIdentifier)
			add("identifier", str)
		elseif c == "\"" then
			local str = nextwhilenm("\"")
			add("literal", str)
			next() -- go past ending quotation
		else
			add("symbol", c)
		end
	end

	return tokens
end

local Node = OOP "Node"
local Expr = OOP("Expression", Node)


local Parser = OOP("Parser")

function Parser:error(str)
	local t = self:curToken()
	local prefix = ""
	if t then
		prefix = string.format("<%s %d:%d> ", t.token, t.line, t.col)
	end

	error(prefix .. str)
end

function Parser:curToken()
	return self.token
end
function Parser:nextToken()
	local t = table.remove(self.tokens, 1)
	self.token = t
	return t
end
function Parser:lookahead(n)
	n = n or 1
	return self.tokens[n]
end
function Parser:peekToken()
	return self:lookahead(1)
end


function Parser:isLastToken()
	return self:peekToken() == nil
end

function Parser:accept(token, str)
	local peeked = self:peekToken()
	if peeked and peeked.token == token and (not str or str == peeked.str) then
		return self:nextToken()
	end
	return false
end
function Parser:expect(token, str)
	local next = self:nextToken()
	if not next or next.token ~= token then
		self:error("Expected '" .. token .. "'. Got " .. tostring(next and next.token))
	end
	if str and next.str ~= str then
		self:error("Expected '" .. token .. "' with str '" .. str .. "'. Got str " .. tostring(next and next.str))
	end
	return next
end

Parser.IfDelimiter = {token = "identifier", str = "then"}
function Parser:expectIfDelimiter()
	self:expect(self.IfDelimiter.token, self.IfDelimiter.str)
end
Parser.BlockStartDelimiter = {token = "identifier", str = "do"}
function Parser:expectBlockStartDelimiter()
	self:expect(self.BlockStartDelimiter.token, self.BlockStartDelimiter.str)
end
Parser.BlockEndDelimiter = {token = "identifier", str = "end"}
function Parser:acceptBlockEndDelimiter()
	return self:accept(self.BlockEndDelimiter.token, self.BlockEndDelimiter.str)
end

function Parser:parseBlock(preParseCheck)
	local stmts = {}

	while true do
		if preParseCheck and preParseCheck() then break end

		local stmt = self:parseStmt()
		if not stmt then break end

		table.insert(stmts, stmt)
	end

	return {type = "Block", stmts = stmts}
end

function Parser:parseIfBody()
	self:expectIfDelimiter()
	return self:parseBlock(function() return self:acceptBlockEndDelimiter() end)
end

function Parser:parseIf()
	local cond = self:parseExpr()

	local node = {type = "IfStatement", cond = cond, body = self:parseIfBody()}

	-- Stupid shit
	if self:accept("identifier", "elseif") then
		local _elseif = self:parseIf()
		_elseif.subif = "elseif"
		node._elseif = _elseif
	elseif self:accept("identifier", "else") then
		local _else = {type = "ElseStatement", body = self:parseIfBody()}
		_else.subif = "else"
		node._else = _else
	end

	return node
end
function Parser:parseForBody()
	self:expectBlockStartDelimiter()
	return self:parseBlock(function() return self:acceptBlockEndDelimiter() end)
end

function Parser:parseFor()
	local name = {type = "Variable", name = self:nextToken().str}
	self:expect("symbol", "=")
	local expr = self:parseExpr()

	local assignment = {type = "VariableAssignment", isLocal = false, lvalue = name, rvalue = expr}

	self:expect("symbol", ",")
	local cond = self:parseExpr()

	local cond2
	if self:accept("symbol", ",") then
		cond2 = self:parseExpr()
	end

	local node = {type = "ForStatement", assignment = assignment, cond = cond, cond2 = cond2, body = self:parseForBody()}
	return node
end

function Parser:parsePrefixExpr()
	local c = self:curToken().str
	local prev = "identifier"
	while true do
		if prev == "identifier" and self:accept("symbol", ".") then
			c = c .. "."
			prev = "symbol"
		elseif prev == "symbol" and self:accept("identifier") then
			c = c .. self:curToken().str
			prev = "identifier"
		else
			break
		end
	end
	return {type = "Variable", name = c}
end

function Parser:parseVariableOrFuncCall()
	local first = self:parsePrefixExpr()
	if self:accept("symbol", "(") then
		local exprlist = self:parseExprList()
		self:expect("symbol", ")")
		return {type = "FunctionCall", func = first, arglist = exprlist}
	end
	return first
end
function Parser:parseAssignmentOrFuncCall(isLocal)
	local first = self:parsePrefixExpr()
	if self:accept("symbol", "(") then
		local exprlist = self:parseExprList()
		self:expect("symbol", ")")
		return {type = "FunctionCall", func = first, arglist = exprlist}
	end
	self:expect("symbol", "=")
	local rvalue = self:parseExpr()
	return {type = "VariableAssignment", isLocal = isLocal, lvalue = first, rvalue = rvalue}
end

function Parser:parseStmt()
	if self:isLastToken() then return nil end

	if self:accept("identifier") then
		local str = self:curToken().str
		if str == "if" then
			return self:parseIf()
		elseif str == "for" then
			return self:parseFor()
		elseif str == "local" then
			if self:accept("identifier", "function") then
				self:error("'local function' syntax is not yet supported.")
			end

			local lvaluetoken = self:expect("identifier")
			return self:parseAssignmentOrFuncCall(true)
		else -- varlist `=´ explist
			return self:parseAssignmentOrFuncCall(false)
		end
	else
		self:error("Expected a statement. Got " .. self:peekToken().token)
	end
end

function Parser:parseVarList()
	local vars = {}

	while true do
		if not self:accept("identifier") then break end

		table.insert(vars, {type = "Variable", name = self:curToken().str})
		if not self:accept("symbol", ",") then break end
	end

	return {type = "VariableList", vars = vars}
end

function Parser:parseExprList()
	local exprs = {}

	while true do
		table.insert(exprs, self:parseExpr())
		if not self:accept("symbol", ",") then break end
	end

	return {type = "ExprList", exprs = exprs}
end

function Parser:parseTableConstructor()
	-- TODO
	self:expect("symbol", "}")
	return {type = "TableConstructor", fields = {}}
end

function Parser:parseExpr()
	local exp1
	if self:accept("identifier", "nil") then
		exp1 = {type = "NilExpression"}
	elseif self:accept("identifier", "true") or self:accept("identifier", "false") then
		exp1 = {type = "BoolExpression", value = self:curToken().str == "true"}
	elseif self:accept("number") then
		exp1 = {type = "NumberExpression", value = tonumber(self:curToken().str)}
	elseif self:accept("literal") then
		exp1 = {type = "StringExpression", value = self:curToken().str}
	elseif self:accept("identifier") then
		exp1 = self:parseVariableOrFuncCall()
	elseif self:accept("symbol", "#") then
		self:nextToken()
		exp1 = {type = "UnaryExpression", op = "#", expr = self:parsePrefixExpr()}
	elseif self:accept("symbol", "{") then
		exp1 = self:parseTableConstructor()
	end

	if exp1 then
		if self:accept("symbol", "*") then
			local exp2 = self:parseExpr()
			return {type = "BinaryExpression", op = "*", expr1 = exp1, expr2 = exp2}
		end
		return exp1
	end
	
	local expr = self:parseExprEx()
	if expr then
		return expr
	else
		self:error("Expected an expression. Got " .. self:peekToken().token)
	end
end
function Parser:parseExprEx()
end

function Parser:parse(tokens)
	self.tokens = tokens
	self.ast = self:parseBlock()
end

local LuaParser = OOP("LuaParser", Parser)

if SERVER then
	local tokens = Lexer.tokenize [[
		local str = "Hello world"
	]]
	local parser = LuaParser()
	parser:parse(tokens)
	print(parser.ast)
end