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
		unVar_(_unVar){
	}
	std::string unVar_;
};
struct BinOp{
	explicit BinOp (const Expr& _left,const std::string& _op,const Expr& _right):
		left_(_left),op_(_op),right_(_right){
	}
	Expr left_;
	std::string op_;
	Expr right_;
};
struct ListOp{
	explicit ListOp (const std::string& _op,const std::vector<Expr >& _args):
		op_(_op),args_(_args){
	}
	std::string op_;
	std::vector<Expr > args_;
};
struct VarCon;
struct BinOpCon;
struct ListOpCon;
typedef boost::variant<boost::recursive_wrapper<VarCon >,boost::recursive_wrapper<BinOpCon >,boost::recursive_wrapper<ListOpCon > > ExprCon;
struct VarCon{
	explicit VarCon (const std::string& _param1):
		param1_(_param1){
	}
	std::string param1_;
};
struct BinOpCon{
	explicit BinOpCon (const ExprCon& _param1,const std::string& _param2,const ExprCon& _param3):
		param1_(_param1),param2_(_param2),param3_(_param3){
	}
	ExprCon param1_;
	std::string param2_;
	ExprCon param3_;
};
struct ListOpCon{
	explicit ListOpCon (const std::string& _param1,const std::vector<ExprCon >& _param2):
		param1_(_param1),param2_(_param2){
	}
	std::string param1_;
	std::vector<ExprCon > param2_;
};
template <typename Ta >
std::vector<Ta > intercalate(const std::vector<Ta >& ,const std::vector<std::vector<Ta > >& );
//~~~~~~~~~~~~~~~~specialization~~~~~~~~~~~~~~~~
std::string intercalate(const std::string& ,const std::vector<std::string >& );
std::vector<int > interint(const std::vector<int >& ,const std::vector<std::vector<int > >& );
int sum(const std::vector<int >& );
template <typename Ta >
std::vector<Ta > concat(const std::vector<std::vector<Ta > >& );
//~~~~~~~~~~~~~~~~specialization~~~~~~~~~~~~~~~~
std::string concat(const std::vector<std::string >& );
std::string s(const Expr& );
std::string s2(const Expr& );
std::string ss(const std::string& ,const Expr& );
int sz(const Expr& );
int opop(const Expr& );
std::string multioccur(const Expr& );
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
template <typename T6 >
std::vector<T6 > intercalate(const std::vector<T6 >& sep,const std::vector<std::vector<T6 > >& _p2){
	return foldl1(std::function<std::function<std::vector<T6 >(const std::vector<T6 >&) >(const std::vector<T6 >&)>([=](const std::vector<T6 >& a){
		return std::function<std::vector<T6 >(const std::vector<T6 >&)>(std::function<std::vector<T6 >(const std::vector<T6 >&)>([=](const std::vector<T6 >& _curry2){
			return chaskell::append(chaskell::append(a,sep),_curry2);
		}));
	}),_p2);
}
//~~~~~~~~~~~~~~~~specialization~~~~~~~~~~~~~~~~
std::string intercalate(const std::string& sep,const std::vector<std::string >& _p2){
	return foldl1(std::function<std::function<std::string(const std::string&) >(const std::string&)>([=](const std::string& a){
		return std::function<std::string(const std::string&)>(std::function<std::string(const std::string&)>([=](const std::string& _curry2){
			return a+sep+_curry2;
		}));
	}),_p2);
}
template <typename T14 >
std::vector<T14 > interint(const std::vector<T14 >& sep,const std::vector<std::vector<T14 > >& l){
	return intercalate(sep,l);
}
int sum(const std::vector<int >& _p1){
	return foldl(std::function<std::function<int(const int&) >(const int&)>([=](const int& _curry1){
		return std::function<int(const int&)>([=](const int& _curry2){
			return _curry1+_curry2;
		});
	}),0,_p1);
}
template <typename T25 >
std::vector<T25 > concat(const std::vector<std::vector<T25 > >& _p1){
	return foldl(std::function<std::function<std::vector<T25 >(const std::vector<T25 >&) >(const std::vector<T25 >&)>([=](const std::vector<T25 >& _curry1){
		return std::function<std::vector<T25 >(const std::vector<T25 >&)>([=](const std::vector<T25 >& _curry2){
			return chaskell::append(_curry1,_curry2);
		});
	}),std::vector<T25>{},_p1);
}
//~~~~~~~~~~~~~~~~specialization~~~~~~~~~~~~~~~~
std::string concat(const std::vector<std::string >& _p1){
	return foldl(std::function<std::function<std::string(const std::string&) >(const std::string&)>([=](const std::string& _curry1){
		return std::function<std::string(const std::string&)>([=](const std::string& _curry2){
			return _curry1+_curry2;
		});
	}),std::string(),_p1);
}
std::string s(const Expr& __param__){
	struct visitor:public boost::static_visitor<std::string >{
		 visitor (){
		}
		std::string operator()(const Var& _var)const{
			return _var.unVar_;
		}
		std::string operator()(const BinOp& _var)const{
			return s(_var.left_)+_var.op_+s(_var.right_);
		}
		std::string operator()(const ListOp& _var)const{
			return _var.op_+std::string("(")+intercalate(std::string(","),map(std::function<std::string(const Expr&)>([=](const Expr& _curry1){
				return s(_curry1);
			}),_var.args_))+std::string(")");
		}
	};
	return boost::apply_visitor(visitor(),__param__);
}
std::string s2(const Expr& v0){
	struct visitor:public boost::static_visitor<std::string >{
		 visitor (){
		}
		std::string operator()(const Var& _var)const{
			return _var.unVar_;
		}
		std::string operator()(const BinOp& _var)const{
			return s(_var.left_)+_var.op_+s(_var.right_);
		}
		std::string operator()(const ListOp& _var)const{
			return _var.op_+std::string("(")+intercalate(std::string(","),map(std::function<std::string(const Expr&)>([=](const Expr& _curry1){
				return s(_curry1);
			}),_var.args_))+std::string(")");
		}
	};
	return boost::apply_visitor(visitor(),v0);
}
std::string ss(const std::string& n,const Expr& __param__){
	struct visitor:public boost::static_visitor<std::string >{
		 visitor (const std::string& _n):
			n(_n){
		}
		std::string n;
		std::string operator()(const Var& _var)const{
			return _var.unVar_;
		}
		std::string operator()(const BinOp& _var)const{
			return ss(n,_var.left_)+_var.op_+ss(n,_var.right_);
		}
		std::string operator()(const ListOp& _var)const{
			return _var.op_+std::string("(")+intercalate(std::string(","),map(std::function<std::string(const Expr&)>([=](const Expr& _curry2){
				return ss(n,_curry2);
			}),_var.args_))+std::string(")");
		}
	};
	return boost::apply_visitor(visitor(n),__param__);
}
int sz(const Expr& __param__){
	struct visitor:public boost::static_visitor<int >{
		 visitor (){
		}
		int operator()(const Var& _var)const{
			return 1;
		}
		int operator()(const BinOp& _var)const{
			return sz(_var.left_)+1+sz(_var.right_);
		}
		int operator()(const ListOp& _var)const{
			return 1+sum(map(std::function<int(const Expr&)>([=](const Expr& _curry1){
				return sz(_curry1);
			}),_var.args_));
		}
	};
	return boost::apply_visitor(visitor(),__param__);
}
int opop(const Expr& __param__){
	struct visitor:public boost::static_visitor<int >{
		 visitor (){
		}
		int operator()(const Var& _var)const{
			return 0;
		}
		int operator()(const BinOp& _var)const{
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
			return boost::apply_visitor(visitor(),_var.left_);
		}
		int operator()(const ListOp& _var)const{
			return 0;
		}
	};
	return boost::apply_visitor(visitor(),__param__);
}
std::string multioccur(const Expr& __param__){
	struct visitor:public boost::static_visitor<std::string >{
		 visitor (){
		}
		std::string operator()(const Var& _var)const{
			std::string v=_var.unVar_;
			return v+v;
		}
		std::string operator()(const BinOp& _var)const{
			return std::string("");
		}
		std::string operator()(const ListOp& _var)const{
			return std::string("");
		}
	};
	return boost::apply_visitor(visitor(),__param__);
}
int letcase(const Expr& __param__){
	struct visitor:public boost::static_visitor<int >{
		 visitor (){
		}
		int operator()(const Var& _var)const{
			return 0;
		}
		int operator()(const BinOp& _var)const{
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
			int x=boost::apply_visitor(visitor(),_var.left_);
			return x+1;
		}
		int operator()(const ListOp& _var)const{
			return 0;
		}
	};
	return boost::apply_visitor(visitor(),__param__);
}
int add(const int& a,const int& b){
	return a+b;
}
int g(const int& x){
	int y=x*x;
	int z=y*y;
	return z*z;
}
template <typename T202 >
T202 letfunc(const T202& x){
	std::function<T202(const T202&) > f=std::function<T202(const T202&)>([=](const T202& x){
		return x;
	});
	return f(x);
}
std::vector<int > append2(const std::vector<int >& x){
	return chaskell::append(x,std::vector<int>{1,2,3});
}

