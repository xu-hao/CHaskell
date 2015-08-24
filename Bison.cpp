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
	explicit VarCon (const std::string& _var1):
		var1_(std::move(_var1)){
	}
	std::string var1_;
};
struct BinOpCon{
	explicit BinOpCon (const ExprCon& _var1,const std::string& _var2,const ExprCon& _var3):
		var1_(std::move(_var1)),var2_(std::move(_var2)),var3_(std::move(_var3)){
	}
	ExprCon var1_;
	std::string var2_;
	ExprCon var3_;
};
struct ListOpCon{
	explicit ListOpCon (const std::string& _var1,const std::vector<ExprCon >& _var2):
		var1_(std::move(_var1)),var2_(std::move(_var2)){
	}
	std::string var1_;
	std::vector<ExprCon > var2_;
};
template <typename Ta >
std::vector<Ta > intercalate(const std::vector<Ta >& ,const std::vector<std::vector<Ta > >& );
std::vector<int > interint(const std::vector<int >& ,const std::vector<std::vector<int > >& );
int sum(const std::vector<int >& );
template <typename Ta >
std::vector<Ta > concat(const std::vector<std::vector<Ta > >& );
std::string s(const Expr& );
std::string s2(const Expr& );
std::string ss(const std::string& ,const Expr& );
int sz(const Expr& );
int opop(const Expr& );
int letcase(const Expr& );
int add(const int& ,const int& );
int g(const int& );
int letfunc(const int& );
std::vector<int > append2(const std::vector<int >& );

//~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#include "chaskell.hpp"
#include <string>
#include <vector>
#include <utility>
#include <boost/variant.hpp>
template <typename Ta >
std::vector<Ta > intercalate(const std::vector<Ta >& sep,const std::vector<std::vector<Ta > >& _param2){
	return foldl1(std::function<std::vector<Ta >(const std::vector<Ta >&,const std::vector<Ta >&)>([=](const std::vector<Ta >& a,const std::vector<Ta >& b){
		return chaskell::append(chaskell::append(a,sep),b);
	}),_param2);
}
std::vector<int > interint(const std::vector<int >& sep,const std::vector<std::vector<int > >& l){
	return intercalate(sep,l);
}
int sum(const std::vector<int >& _param1){
	return foldl(std::function<int(const int&,const int&)>([=](const int& _param1,const int& _param2){
		return (_param1+_param2);
	}),0,_param1);
}
template <typename Ta >
std::vector<Ta > concat(const std::vector<std::vector<Ta > >& _param1){
	return foldl(chaskell::append<std::vector<Ta >,std::vector<Ta >>,std::vector<Ta >{},_param1);
}
std::string s(const Expr& _param1){
	struct visitor:public boost::static_visitor<std::string >{
		 visitor (){
		}
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
std::string s2(const Expr& v0){
	struct visitor:public boost::static_visitor<std::string >{
		 visitor (){
		}
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
	return boost::apply_visitor(visitor(),v0);
}
std::string ss(const std::string& n,const Expr& _param2){
	struct visitor:public boost::static_visitor<std::string >{
		 visitor (const std::string& _n):
			n(_n){
		}
		std::string n;
		std::string operator()(const Var& _var)const{
			auto v=_var.unVar_;
			return v;
		}
		std::string operator()(const BinOp& _var)const{
			auto a=_var.left_;
			auto b=_var.op_;
			auto c=_var.right_;
			return chaskell::append(chaskell::append(ss(n,a),b),ss(n,c));
		}
		std::string operator()(const ListOp& _var)const{
			auto a=_var.op_;
			auto b=_var.args_;
			return chaskell::append(chaskell::append(chaskell::append(a,std::string("(")),intercalate(std::string(","),map(std::function<std::string(const Expr&)>([=](const Expr& _param2){
				return ss(n,_param2);
			}),b))),std::string(")"));
		}
	};
	return boost::apply_visitor(visitor(n),_param2);
}
int sz(const Expr& _param1){
	struct visitor:public boost::static_visitor<int >{
		 visitor (){
		}
		int operator()(const Var& _var)const{
			auto v=_var.unVar_;
			return 1;
		}
		int operator()(const BinOp& _var)const{
			auto a=_var.left_;
			auto b=_var.op_;
			auto c=_var.right_;
			return ((sz(a)+1)+sz(c));
		}
		int operator()(const ListOp& _var)const{
			auto a=_var.op_;
			auto b=_var.args_;
			return (1+sum(map(chaskell::function(sz),b)));
		}
	};
	return boost::apply_visitor(visitor(),_param1);
}
int opop(const Expr& _param1){
	struct visitor:public boost::static_visitor<int >{
		 visitor (){
		}
		int operator()(const Var& _var)const{
			auto v=_var.unVar_;
			return 0;
		}
		int operator()(const BinOp& _var)const{
			auto a=_var.left_;
			auto b=_var.op_;
			auto c=_var.right_;
			struct visitor:public boost::static_visitor<int >{
				 visitor (){
				}
				int operator()(const Var& _var)const{
					return 0;
				}
				int operator()(const BinOp& _var)const{
					return 1;
				}
				int operator()(const ListOp& _var)const{
					return 0;
				}
			};
			return boost::apply_visitor(visitor(),a);
		}
		int operator()(const ListOp& _var)const{
			auto a=_var.op_;
			auto b=_var.args_;
			return 0;
		}
	};
	return boost::apply_visitor(visitor(),_param1);
}
int letcase(const Expr& _param1){
	struct visitor:public boost::static_visitor<int >{
		 visitor (){
		}
		int operator()(const Var& _var)const{
			auto v=_var.unVar_;
			return 0;
		}
		int operator()(const BinOp& _var)const{
			auto a=_var.left_;
			auto b=_var.op_;
			auto c=_var.right_;
			struct visitor:public boost::static_visitor<int >{
				 visitor (){
				}
				int operator()(const Var& _var)const{
					auto v=_var.unVar_;
					return 0;
				}
				int operator()(const BinOp& _var)const{
					auto a=_var.left_;
					auto b=_var.op_;
					auto c=_var.right_;
					return 1;
				}
				int operator()(const ListOp& _var)const{
					auto a=_var.op_;
					auto b=_var.args_;
					return 0;
				}
			};
			int x=boost::apply_visitor(visitor(),a);
			return (x+1);
		}
		int operator()(const ListOp& _var)const{
			auto a=_var.op_;
			auto b=_var.args_;
			return 0;
		}
	};
	return boost::apply_visitor(visitor(),_param1);
}
int add(const int& a,const int& b){
	return (a+b);
}
int g(const int& x){
	int y=(x*x);
	int z=(y*y);
	return (z*z);
}
int letfunc(const int& x){
	std::function<int(const int&)> f=[=](const int& x){
		return x;
	};
	return f(x);
}
std::vector<int > append2(const std::vector<int >& x){
	return chaskell::append(x,std::vector<int >{1,2,3});
}

