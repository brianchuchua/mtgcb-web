import { combineReducers } from '@reduxjs/toolkit';
import browseSliceReducer from '../features/browse/browseSlice';

const rootReducer = combineReducers({
  browse: browseSliceReducer,
});

export type RootState = ReturnType<typeof rootReducer>;

export default rootReducer;
