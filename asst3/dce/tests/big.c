void foo(){
    int z = 0;
    for (int x = 0; x < 10; x++){
      for(int y = 0; y < 15; y++){
        x = y+3;
        y = x+3;
      }
    }

    return;
}
