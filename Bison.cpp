#include "chaskell.hpp"
#include <string>
#include <vector>
#include <utility>
#include <boost/variant.hpp>
struct Var;
struct BinOp;
struct ListOp;
typedef boost::variant<boost::recursive_wrapper<Var >,boost::recursive_wrapper<BinOp >,boost::recursive_wrapper<ListOp > > Expr;
struct Var{
	explicit Var (const std::string& _unVar):
		unVar_(std::move(_unVar)){
	}
	std::string unVar_;
};
struct BinOp{
	explicit BinOp (const Expr& _left,const std::string& _op,const Expr& _right):
		left_(std::move(_left)),op_(std::move(_op)),right_(std::move(_right)){
	}
	Expr left_;
	std::string op_;
	Expr right_;
};
struct ListOp{
	explicit ListOp (const std::string& _op,const std::vector<Expr >& _args):
		op_(std::move(_op)),args_(std::move(_args)){
	}
	std::string op_;
	std::vector<Expr > args_;
};
struct VarCon;
struct BinOpCon;
struct ListOpCon;
typedef boost::variant<boost::recursive_wrapper<VarCon >,boost::recursive_wrapper<BinOpCon >,boost::recursive_wrapper<ListOpCon > > ExprCon;
struct VarCon{
	explicit VarCon (const std::string& _arg1):
		arg1_(std::move(_arg1)){
	}
	std::string arg1_;
};
struct BinOpCon{
	explicit BinOpCon (const ExprCon& _arg1,const std::string& _arg2,const ExprCon& _arg3):
		arg1_(std::move(_arg1)),arg2_(std::move(_arg2)),arg3_(std::move(_arg3)){
	}
	ExprCon arg1_;
	std::string arg2_;
	ExprCon arg3_;
};
struct ListOpCon{
	explicit ListOpCon (const std::string& _arg1,const std::vector<ExprCon >& _arg2):
		arg1_(std::move(_arg1)),arg2_(std::move(_arg2)){
	}
	std::string arg1_;
	std::vector<ExprCon > arg2_;
};
template <typename Ta >
std::vector<Ta > intercalate(const std::vector<Ta >& ,const std::vector<std::vector<Ta > >& );
int sum(const std::vector<int >& );
std::string s(const Expr& );
int sz(const Expr& );
int add(const int& ,const int& );

//~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#include "chaskell.hpp"
#include <string>
#include <vector>
#include <utility>
#include <boost/variant.hpp>
template <typename Ta >
std::vector<Ta > intercalate(const std::vector<Ta >& sep,const std::vector<std::vector<Ta > >& _param2){
	return foldl1([=](auto a,auto b){
		return chaskell::append(chaskell::append(a,sep),b);
	},_param2);
}
int sum(const std::vector<int >& _param1){
	return foldl(chaskell::function(chaskell::add),0,_param1);
}
std::string s(const Expr& _param1){
	struct visitor:public boost::static_visitor<std::string >{
		std::string operator()(const Var& _var)const{
			auto v=_var.unVar_;
			return v;
		}
		std::string operator()(const BinOp& _var)const{
			auto a=_var.left_;
			auto b=_var.op_;
			auto c=_var.right_;
			return chaskell::append(chaskell::append(s(a),b),s(c));
		}
		std::string operator()(const ListOp& _var)const{
			auto a=_var.op_;
			auto b=_var.args_;
			return chaskell::append(chaskell::append(chaskell::append(a,std::string("(")),intercalate(std::string(","),map(chaskell::function(s),b))),std::string(")"));
		}
	};
	return boost::apply_visitor(visitor(),_param1);
}
int sz(const Expr& _param1){
	struct visitor:public boost::static_visitor<int >{
		int operator()(const Var& _var)const{
			auto v=_var.unVar_;
			return 1;
		}
		int operator()(const BinOp& _var)const{
			auto a=_var.left_;
			auto b=_var.op_;
			auto c=_var.right_;
			return chaskell::add(chaskell::add(sz(a),1),sz(c));
		}
		int operator()(const ListOp& _var)const{
			auto a=_var.op_;
			auto b=_var.args_;
			return chaskell::add(1,sum(map(chaskell::function(sz),b)));
		}
	};
	return boost::apply_visitor(visitor(),_param1);
}
int add(const int& a,const int& b){
	return chaskell::add(a,b);
}

