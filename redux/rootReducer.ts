import { combineReducers } from '@reduxjs/toolkit';
import browseSliceReducer from '../features/browse/browseSlice';
import { mtgcbApi } from '../network/services/mtgcbApi';

const rootReducer = combineReducers({
  browse: browseSliceReducer,
  [mtgcbApi.reducerPath]: mtgcbApi.reducer,
});

export type RootState = ReturnType<typeof rootReducer>;

export default rootReducer;
