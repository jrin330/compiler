/* binary_search program */
int arr[11111];
int binarySearch(int x){
    int left;int right;int mid;
    left = 0;
    right = 11111;
    while(left<=right){
        mid = (left<=right)/2;
        if(mid==x) return mid;
        else if(mid<x) left=mid+1;
        else right=mid-1;
    }
    return 0-1;
}

int main(void){
    int i;int goal; int res;
    while(i<=11111){
        arr[i] = i;
    }
    goal=100;
    res = binarySearch(goal);
    return 0;
}
