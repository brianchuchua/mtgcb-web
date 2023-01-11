import FormControl from '@material-ui/core/FormControl';
import InputAdornment from '@material-ui/core/InputAdornment';
import InputLabel from '@material-ui/core/InputLabel';
import OutlinedInput from '@material-ui/core/OutlinedInput';
import SearchIcon from '@material-ui/icons/Search';
import debounce from 'lodash.debounce';
import { useCallback } from 'react';
import styled from 'styled-components';
import { searchFieldDebounceTimeMs } from '../../util/useDebounce';

interface ArtistSearchProps {
  artistQuery: string;
  updateArtistQuery: (query: string) => void;
}

const debouncedCallback = debounce((callback, query) => callback(query), searchFieldDebounceTimeMs);

const ArtistSearch: React.FC<ArtistSearchProps> = ({ artistQuery, updateArtistQuery }) => (
  <StyledArtistSearch fullWidth variant="outlined">
    <InputLabel htmlFor="artist-query" className="input-label-fix">
      Artist
    </InputLabel>
    <OutlinedInput
      id="artist-query"
      defaultValue={artistQuery}
      placeholder="Search by artist"
      label="Artist"
      onChange={useCallback((e) => debouncedCallback(updateArtistQuery, e.target.value), [])}
      startAdornment={
        <InputAdornment position="start">
          <SearchIcon color="disabled" />
        </InputAdornment>
      }
    />
  </StyledArtistSearch>
);

const StyledArtistSearch = styled(FormControl)(() => ({
  paddingLeft: '8px',
  paddingRight: '8px',
  paddingBottom: '10px',
}));

export default ArtistSearch;
