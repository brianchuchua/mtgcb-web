import { createSlice, PayloadAction } from '@reduxjs/toolkit';

interface BrowseState {
  isFormVisible: boolean;
  searchQuery: string;
}

const initialState: BrowseState = {
  isFormVisible: false,
  searchQuery: '',
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
  },
});

export const { setFormVisibility, setSearchQuery } = browseSlice.actions;

export default browseSlice.reducer;
