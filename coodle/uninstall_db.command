echo "create user xxx" | psql postgres
echo "reassign owned by coodle to xxx" | psql postgres
echo "drop database coodle_data" | psql postgres
echo "drop user coodle" | psql postgres
brew services stop postgresql
brew uninstall postgresql
