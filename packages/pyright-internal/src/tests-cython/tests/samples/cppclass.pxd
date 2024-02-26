# distutils: language = c++
from libcpp.vector cimport vector
cdef extern from *:
    """
    #include <vector>
    template<typename TV>
    class Point
    {
    private:
        int m_x, m_y;
        std::vector<TV> m_v;
    public:
        Point();
        Point(int x, int y);
        ~Point();
        operator bool(){return true;};
        int operator[](int index){return 0;};
        bool operator==(Point&);
        inline int x() {return m_x;};
        inline int y() {return m_y;};
        std::vector<TV> vec() {return m_v;};
        void setVec(std::vector<TV>& v) {m_v = v;};
    };
    """
    cdef cppclass Point[TV]:
        Point()
        Point(int, int)
        bint operator bool()
        int operator[](int)
        int x()
        int y()
        vector[TV]& vec()
        void setVec(vector[TV]&)


