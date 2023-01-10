import FormControl from '@material-ui/core/FormControl';
import InputAdornment from '@material-ui/core/InputAdornment';
import InputLabel from '@material-ui/core/InputLabel';
import OutlinedInput from '@material-ui/core/OutlinedInput';
import SearchIcon from '@material-ui/icons/Search';
import styled from 'styled-components';

interface ArtistSearchProps {
  artistQuery: string;
  updateArtistQuery: (query: string) => void;
}

const ArtistSearch: React.FC<ArtistSearchProps> = ({ artistQuery, updateArtistQuery }) => (
  <StyledArtistSearch fullWidth variant="outlined">
    <InputLabel htmlFor="artist-query" className="input-label-fix">
      Artist
    </InputLabel>
    <OutlinedInput
      id="artist-query"
      value={artistQuery}
      placeholder="Search by artist"
      label="Artist"
      onChange={(e) => updateArtistQuery(e.target.value)}
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
