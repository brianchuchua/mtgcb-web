import { combineReducers } from '@reduxjs/toolkit';
import browseSliceReducer from '../features/browse/browseSlice';
import collectionSliceReducer from '../features/collections/collectionSlice';
import setCollectionSliceReducer from '../features/collections/sets/setCollectionSlice';
import setSliceReducer from '../features/sets/setSlice';
import { mtgcbApi } from '../network/services/mtgcbApi';

const rootReducer = combineReducers({
  browse: browseSliceReducer,
  collection: collectionSliceReducer,
  setCollection: setCollectionSliceReducer,
  set: setSliceReducer,
  [mtgcbApi.reducerPath]: mtgcbApi.reducer,
});

export type RootState = ReturnType<typeof rootReducer>;

export default rootReducer;
