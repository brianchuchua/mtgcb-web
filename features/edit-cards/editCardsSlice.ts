import { createSlice, PayloadAction } from '@reduxjs/toolkit';

const initialState: EditCardsState = {
  priceType: 'market',
  cardEditingMode: 'increment',
  isFormVisible: false,
};

const editCardsSlice = createSlice({
  name: 'editCards',
  initialState,
  reducers: {
    setPriceType(state, action: PayloadAction<PriceTypes>) {
      const priceType = action.payload;
      state.priceType = priceType;
    },
    setCardEditingMode(state, action: PayloadAction<CardEditingModes>) {
      const cardEditingMode = action.payload;
      state.cardEditingMode = cardEditingMode;
    },
    setFormVisibility(state, action: PayloadAction<FormVisibility>) {
      const { isFormVisibile } = action.payload;
      state.isFormVisible = isFormVisibile;
    },
  },
});

export const { setPriceType, setCardEditingMode, setFormVisibility } = editCardsSlice.actions;

interface EditCardsState {
  priceType: PriceTypes;
  cardEditingMode: CardEditingModes;
  isFormVisible: boolean;
}

export type PriceTypes = 'low' | 'average' | 'high' | 'market' | 'foil';

export type CardEditingModes = 'increment' | 'set';

interface FormVisibility {
  isFormVisibile: boolean;
}

export default editCardsSlice.reducer;
