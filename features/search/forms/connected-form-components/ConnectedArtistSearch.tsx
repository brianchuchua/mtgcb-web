import { useDispatch, useSelector } from 'react-redux';
import ArtistSearch from '../../../../components/search-form-components/ArtistSearch';
import { RootState } from '../../../../redux/rootReducer';
import { useQueryParameter } from '../../../../util/useQueryParameter';
import { ConnectedSearchFormComponentProps } from './types';

interface ConnectedArtistSearchProps extends ConnectedSearchFormComponentProps {
  setArtistQuery: any;
}

const ConnectedArtistSearch: React.FC<ConnectedArtistSearchProps> = ({ reduxSlice, setArtistQuery }) => {
  const dispatch = useDispatch();
  const { artistQuery } = useSelector((state: RootState) => state[reduxSlice]);
  const setQueryParameter = useQueryParameter();

  const updateArtistQuery = (newArtistQuery: string) => {
    dispatch(setArtistQuery({ artistQuery: newArtistQuery }));
    setQueryParameter('artist', newArtistQuery);
  };

  return <ArtistSearch artistQuery={artistQuery} updateArtistQuery={updateArtistQuery} />;
};

export default ConnectedArtistSearch;
