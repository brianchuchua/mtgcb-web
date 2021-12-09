import { combineReducers } from '@reduxjs/toolkit';
import browseSliceReducer from '../features/browse/browseSlice';
import setSliceReducer from '../features/sets/setSlice';
import { mtgcbApi } from '../network/services/mtgcbApi';

const rootReducer = combineReducers({
  browse: browseSliceReducer,
  set: setSliceReducer,
  [mtgcbApi.reducerPath]: mtgcbApi.reducer,
});

export type RootState = ReturnType<typeof rootReducer>;

export default rootReducer;
