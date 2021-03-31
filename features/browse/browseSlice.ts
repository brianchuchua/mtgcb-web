import { createSlice, PayloadAction } from '@reduxjs/toolkit';

export interface CardType {
  categoryLabel: string;
  cardType: string;
  exclude: boolean;
}
interface BrowseState {
  isFormVisible: boolean;
  searchQuery: string;
  cardTypes: CardType[];
}

export interface CardTypes {
  cardTypes: CardType[];
}

const initialState: BrowseState = {
  isFormVisible: false,
  searchQuery: '',
  cardTypes: [],
};

interface FormVisibility {
  isFormVisibile: boolean;
}

interface SearchQuery {
  searchQuery: string;
}

const browseSlice = createSlice({
  name: 'browse',
  initialState,
  reducers: {
    setFormVisibility(state, action: PayloadAction<FormVisibility>) {
      const { isFormVisibile } = action.payload;
      state.isFormVisible = isFormVisibile;
    },
    setSearchQuery(state, action: PayloadAction<SearchQuery>) {
      const { searchQuery } = action.payload;
      state.searchQuery = searchQuery;
    },
    setCardTypes(state, action: PayloadAction<CardTypes>) {
      const { cardTypes } = action.payload;
      state.cardTypes = cardTypes;
    },
  },
});

export const { setFormVisibility, setSearchQuery, setCardTypes } = browseSlice.actions;

export default browseSlice.reducer;
