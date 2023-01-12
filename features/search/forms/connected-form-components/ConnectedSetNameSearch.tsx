import FormControl from '@material-ui/core/FormControl';
import InputAdornment from '@material-ui/core/InputAdornment';
import InputLabel from '@material-ui/core/InputLabel';
import OutlinedInput from '@material-ui/core/OutlinedInput';
import SearchIcon from '@material-ui/icons/Search';
import debounce from 'lodash.debounce';
import { useCallback } from 'react';
import { useDispatch, useSelector } from 'react-redux';
import styled from 'styled-components';
import { RootState } from '../../../../redux/rootReducer';
import { searchFieldDebounceTimeMs } from '../../../../util/useDebounce';
import { ConnectedSearchFormComponentProps } from './types';

interface ConnectedSetNameSearchProps extends ConnectedSearchFormComponentProps {
  setExpansionSearchQuery: any;
}

const debouncedCallback = debounce((callback, query) => callback(query), searchFieldDebounceTimeMs);

const ConnectedSetNameSearch: React.FC<ConnectedSetNameSearchProps> = ({ reduxSlice, setExpansionSearchQuery }) => {
  const dispatch = useDispatch();
  const { expansionSearchQuery } = useSelector((state: RootState) => state[reduxSlice]);

  const updateSearchQuery = (newSearchQuery: string) => {
    dispatch(setExpansionSearchQuery({ expansionSearchQuery: newSearchQuery }));
  };

  return (
    <StyledSetNameSearch fullWidth variant="outlined">
      <InputLabel htmlFor="expansion-search-query" className="input-label-fix">
        Set
      </InputLabel>
      <OutlinedInput
        id="expansion-search-query"
        defaultValue={expansionSearchQuery}
        placeholder="Search by set name or code"
        label="Set"
        onChange={useCallback((e) => debouncedCallback(updateSearchQuery, e.target.value), [])}
        startAdornment={
          <InputAdornment position="start">
            <SearchIcon color="disabled" />
          </InputAdornment>
        }
      />
    </StyledSetNameSearch>
  );
};

const StyledSetNameSearch = styled(FormControl)(() => ({
  marginTop: '5px',
  paddingLeft: '8px',
  paddingRight: '8px',
  paddingBottom: '10px',
}));

export default ConnectedSetNameSearch;
