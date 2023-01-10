import { useDispatch, useSelector } from 'react-redux';
import ArtistSearch from '../../../../components/search-form-components/ArtistSearch';
import { RootState } from '../../../../redux/rootReducer';
import { ConnectedSearchFormComponentProps } from './types';

interface ConnectedArtistSearchProps extends ConnectedSearchFormComponentProps {
  setArtistQuery: any;
}

const ConnectedArtistSearch: React.FC<ConnectedArtistSearchProps> = ({ reduxSlice, setArtistQuery }) => {
  const dispatch = useDispatch();
  const { artistQuery } = useSelector((state: RootState) => state[reduxSlice]);

  const updateArtistQuery = (newArtistQuery: string) => {
    dispatch(setArtistQuery({ artistQuery: newArtistQuery }));
  };

  return <ArtistSearch artistQuery={artistQuery} updateArtistQuery={updateArtistQuery} />;
};

export default ConnectedArtistSearch;
