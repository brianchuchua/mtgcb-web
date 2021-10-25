import { configureStore } from '@reduxjs/toolkit';
import { mtgcbApi } from '../network/services/mtgcbApi';
import rootReducer from './rootReducer';

const store = configureStore({
  reducer: rootReducer,
  middleware: (getDefaultMiddleware) => getDefaultMiddleware().concat(mtgcbApi.middleware),
});

export type AppDispatch = typeof store.dispatch;

export default store;
