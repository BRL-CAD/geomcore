/*                      C O M B I N A T I O N . H
 * BRL-CAD
 *
 * Copyright (c) 2008-2011 United States Government as represented by
 * the U.S. Army Research Laboratory.
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public License
 * version 2.1 as published by the Free Software Foundation.
 *
 * This library is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this file; see the file named COPYING for more
 * information.
 */
/** @file Combination.h
 *
 *  BRL-CAD core C++ interface:
 *      combination (ID_COMBINATION) database object declaration
 *
 *  Origin -
 *      TNO (Netherlands)
 *      IABG mbH (Germany)
 */

#ifndef BRLCAD_COMBINATION_INCLUDED
#define BRLCAD_COMBINATION_INCLUDED

#include <brlcad/Object.h>


struct rt_comb_internal;
union tree;


namespace BRLCAD {
    class BRLCAD_COREINTERFACE_EXPORT Combination : public Object {
    public:
        Combination(void) throw(bad_alloc);
        Combination(const Combination& original) throw(bad_alloc);
        virtual ~Combination(void) throw();

        const Combination&    operator=(const Combination& original) throw(bad_alloc);
        
        class TreeNode; // part 1/2 of a work-around a bug in i686-apple-darwin9-gcc-4.0.1 (GCC) 4.0.1 (Apple Inc. build 5465)

        class BRLCAD_COREINTERFACE_EXPORT ConstTreeNode {
        public:
            enum Operator {
                Null,         ///< the empty tree of an empty combination, may signal an error too e.g. if an operand of a leaf was requested
                Union,        ///< a binary operation, the set theoretic union set of the two operands
                Intersection, ///< a binary operation, the set theoretic intersection set of the two operands
                Subtraction,  ///< a binary operation, the set theoretic complement or difference set of the two operands
                ExclusiveOr,  ///< a binary operation, the set theoretic symmetric difference set of the two operands
                Not,          ///< a unary operation, the complement with the whole space
                Leaf          ///< actually an operand
            };

            ConstTreeNode(const ConstTreeNode& original) throw() : m_tree(original.m_tree) {}
            virtual ~ConstTreeNode(void) throw() {}

            const ConstTreeNode& operator=(const ConstTreeNode& original) throw() {
                m_tree = original.m_tree;
                return *this;
            }

            Operator             Operation(void) const throw();
            ConstTreeNode        LeftOperand(void) const throw();  ///< the left operand of a binary operation
            ConstTreeNode        RightOperand(void) const throw(); ///< the right operand of a binary operation
            ConstTreeNode        Operand(void) const throw();      ///< the operand of a unary operation
            const char*          Name(void) const throw();         ///< the name of the operand object in a leaf
            const double*        Matrix(void) const throw();       ///< the transformation matrix of the operand object in a leaf

        protected:
            tree* m_tree;

            ConstTreeNode(void) throw() : m_tree(0) {}
            ConstTreeNode(tree* original) throw() : m_tree(original) {}

            friend class TreeNode; // part 2/2 of a work-around a bug in i686-apple-darwin9-gcc-4.0.1 (GCC) 4.0.1 (Apple Inc. build 5465)
            friend class Combination;
        };


        class BRLCAD_COREINTERFACE_EXPORT TreeNode : public ConstTreeNode {
        public:
            TreeNode(const TreeNode& original) throw() : ConstTreeNode(original),
                                                         m_internalp(original.m_internalp),
                                                         m_resp(original.m_resp) {}
            virtual ~TreeNode(void) throw() {}

            const TreeNode& operator=(const TreeNode& original) throw() {
                ConstTreeNode::operator=(original);

                m_internalp = original.m_internalp;
                m_resp      = original.m_resp;

                return *this;
            }

            TreeNode        LeftOperand(void) throw();                          ///< the left operand of a binary operation
            TreeNode        RightOperand(void) throw();                         ///< the right operand of a binary operation
            TreeNode        Operand(void) throw();                              ///< the operand of a unary operation
            void            SetName(const char* value) const throw(bad_alloc);  ///< sets the name of the operand object in a leaf
            void            SetMatrix(double value[16]) const throw(bad_alloc); ///< sets the transformation matrix of the operand object in a leaf

            /// applies an operator to this node
            /** - it creates a new node
                - the operator has to be unary, i.e. \a Not
                - \return the new created operation node
            */
            TreeNode        Apply(Operator op) throw(bad_alloc);

            /// creates a new node with "this op theOther"
            /** - the operator has to be binary
                - this node becomes a child of the new created one
                - \return the new created operation node
            */
            TreeNode        Apply(Operator             op,
                                  const ConstTreeNode& theOther) throw(bad_alloc);

            /// "this op leaf(\a leafName)"
            /** - creates two nodes
                  - a leaf node using \a leafName
                  - a operation node
                - the operator has to be binary
                - this node becomes a child of the new created operation node
                - \return the new created operation node
            */
            TreeNode        Apply(Operator    op,
                                  const char* leafName) throw(bad_alloc);

            /// creates a new node with "theOther op this"
            /** - the operator has to be binary
                - this node becomes a child of the new created one
                - \return the new created operation node
            */
            TreeNode        Apply(const ConstTreeNode& theOther,
                                  Operator             op) throw(bad_alloc);

            /// "this op leaf(\a leafName)"
            /** - creates two nodes
                  - a leaf node using \a leafName
                  - a operation node
                - the operator has to be binary
                - this node becomes a child of the new created operation node
                - \return the new created operation node
            */
            TreeNode        Apply(const char* leafName,
                                  Operator    op) throw(bad_alloc);

            /// deletes this node and rewinds all parint nodes until a valid state is reached
            /** E.g. if this node is an operand of a binary operation, this operation will be removed too
                and the other operand of this binary operation becomes the new child node of the former binary operations parent node.

                This node becomes invalid (operator \a Null) during this procedure.
            */
            void            Delete(void) throw();

        private:
            rt_comb_internal* m_internalp;
            resource*         m_resp;

            TreeNode(void) throw() : ConstTreeNode(), m_internalp(0), m_resp(0) {}
            TreeNode(tree*             original,
                     rt_comb_internal* internalp,
                     resource*         resp) throw() : ConstTreeNode(original), m_internalp(internalp), m_resp(resp) {}

            friend class Combination;
        };


        ConstTreeNode         Tree(void) const throw();
        TreeNode              Tree(void) throw();

        /// adds a leaf to the tree
        /** If the tree's root node is not \a Null \a Tree().Apply(Union, leafName) will be performed */
        void                  AddLeaf(const char* leafName) throw(bad_alloc);

        bool                  IsRegion(void) const throw();
        void                  SetIsRegion(bool value) throw();

        enum FastgenType {
            Non,   ///< not a Fastgen region
            Plate,
            Volume
        };

        FastgenType           FastgenRegion(void) const throw();
        void                  SetFastgenRegion(FastgenType value) throw();
        int                   RegionId(void) const throw();
        void                  SetRegionId(int value) throw();
        int                   Aircode(void) const throw();
        void                  SetAircode(int value) throw();
        int                   GiftMaterial(void) const throw();
        void                  SetGiftMaterial(int value) throw();
        int                   LineOfSight(void) const throw();
        void                  SetLineOfSight(int value) throw();
        bool                  HasColor(void) const throw();
        void                  SetHasColor(bool value) throw();
        double                Red(void) const throw();
        void                  SetRed(double value) throw();
        double                Green(void) const throw();
        void                  SetGreen(double value) throw();
        double                Blue(void) const throw();
        void                  SetBlue(double value) throw();
        const char*           Shader(void) const throw();
        void                  SetShader(const char* value) throw(bad_alloc);
        bool                  Inherit(void) const throw(); ///< override lower nodes color and shader
        void                  SetInherit(bool value) throw();
        const char*           Material(void) const throw();
        void                  SetMaterial(const char* value) throw(bad_alloc);
        double                Temperature(void) const throw();
        void                  SetTemperature(double value) throw();

        // inherited from BRLCAD::Object
        virtual const Object& operator=(const Object& original) throw(bad_alloc);
        virtual Object*       Clone(void) const throw(bad_alloc, std::bad_alloc);
        static const char*    ClassName(void) throw();
        virtual const char*   Type(void) const throw();
        virtual bool          IsValid(void) const throw();

    protected:
        Combination(resource*       resp,
                    directory*      pDir,
                    rt_db_internal* ip,
                    db_i*           dbip = 0) throw();

        friend class ConstDatabase;

    private:
        // holds Objects's content if not connected to a database
        rt_comb_internal* m_internalp;

        const rt_comb_internal*   Internal(void) const throw();
        virtual rt_comb_internal* Internal(void) throw();

        friend class Database;
    };
}


#endif // BRLCAD_COMBINATION_INCLUDED

/*
 * Local Variables:
 * tab-width: 8
 * mode: C
 * indent-tabs-mode: t
 * c-file-style: "stroustrup"
 * End:
 * ex: shiftwidth=4 tabstop=8
 */
