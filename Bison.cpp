#include <string>
#include <vector>
#include <utility>
#include <boost/variant.hpp>
struct Var;
struct BinOp;
struct ListOp;
typedef boost::variant<boost::recursive_wrapper<Var >,boost::recursive_wrapper<BinOp >,boost::recursive_wrapper<ListOp > > Expr;
struct Var{
	explicit Var (std::string _unVar):
		unVar_((std::move)(_unVar)){
	}
	std::string unVar_;
};
struct BinOp{
	explicit BinOp (Expr _left,std::string _op,Expr _right):
		left_((std::move)(_left)),op_((std::move)(_op)),right_((std::move)(_right)){
	}
	Expr left_;
	std::string op_;
	Expr right_;
};
struct ListOp{
	explicit ListOp (std::string _op,std::vector<Expr > _args):
		op_((std::move)(_op)),args_((std::move)(_args)){
	}
	std::string op_;
	std::vector<Expr > args_;
};
struct VarCon;
struct BinOpCon;
struct ListOpCon;
typedef boost::variant<boost::recursive_wrapper<VarCon >,boost::recursive_wrapper<BinOpCon >,boost::recursive_wrapper<ListOpCon > > ExprCon;
struct VarCon{
	explicit VarCon (std::string _arg1):
		arg1_((std::move)(_arg1)){
	}
	std::string arg1_;
};
struct BinOpCon{
	explicit BinOpCon (Expr _arg1,std::string _arg2,Expr _arg3):
		arg1_((std::move)(_arg1)),arg2_((std::move)(_arg2)),arg3_((std::move)(_arg3)){
	}
	Expr arg1_;
	std::string arg2_;
	Expr arg3_;
};
struct ListOpCon{
	explicit ListOpCon (std::string _arg1,std::vector<Expr > _arg2):
		arg1_((std::move)(_arg1)),arg2_((std::move)(_arg2)){
	}
	std::string arg1_;
	std::vector<Expr > arg2_;
};

