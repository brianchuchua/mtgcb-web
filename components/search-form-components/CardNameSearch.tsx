import FormControl from '@material-ui/core/FormControl';
import InputAdornment from '@material-ui/core/InputAdornment';
import InputLabel from '@material-ui/core/InputLabel';
import OutlinedInput from '@material-ui/core/OutlinedInput';
import SearchIcon from '@material-ui/icons/Search';
import debounce from 'lodash.debounce';
import { useCallback } from 'react';
import styled from 'styled-components';
import { searchFieldDebounceTimeMs } from '../../util/useDebounce';

interface CardNameSearchProps {
  searchQuery: string;
  updateSearchQuery: (query: string) => void;
}

const debouncedCallback = debounce((callback, query) => callback(query), searchFieldDebounceTimeMs);

const CardNameSearch: React.FC<CardNameSearchProps> = ({ searchQuery, updateSearchQuery }) => (
  <StyledCardNameSearch fullWidth variant="outlined">
    <InputLabel htmlFor="search-query" className="input-label-fix">
      Card Name
    </InputLabel>
    <OutlinedInput
      id="search-query"
      defaultValue={searchQuery}
      placeholder="Search by card name"
      label="Card Name"
      onChange={useCallback((e) => debouncedCallback(updateSearchQuery, e.target.value), [])}
      startAdornment={
        <InputAdornment position="start">
          <SearchIcon color="disabled" />
        </InputAdornment>
      }
    />
  </StyledCardNameSearch>
);

const StyledCardNameSearch = styled(FormControl)(() => ({
  paddingLeft: '8px',
  paddingRight: '8px',
  paddingBottom: '10px',
}));

export default CardNameSearch;
